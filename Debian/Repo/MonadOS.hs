{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS(getOS, putOS, modifyOS)
    , evalMonadOS
    , updateLists
    , aptGetInstall
    , syncLocalPool
    , osFlushPackageCache
    , buildEssential
    , Debian.Repo.MonadOS.syncOS
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, pure, (<$>))
#endif
import Control.DeepSeq (force)
import Control.Exception ({-evaluate,-} SomeException)
import Control.Lens (set, view)
import Control.Monad.Catch (mask_, MonadMask)
import Control.Monad.State (MonadState(get), StateT, evalStateT, get)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Control.Monad.Trans.Except () -- instances
import Data.ByteString.Lazy as L (ByteString, empty)
import Data.Traversable
import Debian.Pretty (ppShow)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(..), envPath, envRoot, EnvRoot(..), rootPath)
import Debian.Repo.Internal.Repos (MonadRepos, osFromRoot, putOSImage, syncOS)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.OSImage as OS (OSImage(..), osRoot, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache)
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude.Process (readProcessVE, readProcessV, readProcessQE)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Prelude hiding (mapM, sequence)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (proc)
import System.Unix.Chroot (useEnv)
import System.Unix.Mount (withProcAndSys, WithProcAndSys)

-- | The problem with having an OSImage in the state of MonadOS is
-- that then we are modifying a copy of the OSImage in MonadRepos, we
-- want to go into MonadRepos and modify the map element there.  So
-- instead put an EnvRoot to look up the OSImage.
class (Monad m, Functor m) => MonadOS m where
    getOS :: m OSImage
    putOS :: OSImage -> m ()
    modifyOS :: (OSImage -> OSImage) -> m ()

instance MonadRepos m => MonadOS (StateT EnvRoot m) where
    getOS = get >>= \ root -> maybe (error "getOS") id <$> (lift $ osFromRoot root)
    putOS = lift . putOSImage
    modifyOS f = getOS >>= putOS . f

instance MonadOS m => MonadOS (WithProcAndSys m) where
    getOS = lift getOS
    putOS = lift . putOS
    modifyOS f = lift $ getOS >>= putOS . f

-- | Perform a task in the changeroot of an OS.
useOS :: (MonadOS m, MonadIO m, MonadMask m) => IO a -> m a
useOS action =
  do root <- view (osRoot . rootPath) <$> getOS
     withProcAndSys root $ liftIO $ useEnv root (return {-. force-}) action

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadRepos m => StateT EnvRoot m a -> EnvRoot -> m a
evalMonadOS task root = evalStateT task root

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: forall m. (Applicative m, MonadOS m, MonadIO m, MonadMask m) => m Bool
updateLists = do
  r1 <- update >>= f
  r2 <- if r1 then pure True else or <$> sequence [aptinstall >>= f, configure >>= f, update >>= f]
  if r2 then mask_ upgrade >>= f else pure False
    where
      f :: (Either SomeException (ExitCode, ByteString, ByteString)) -> m Bool
      f (Right (ExitSuccess, _, _)) = return True
      f _ = return False
      update :: m (Either SomeException (ExitCode, ByteString, ByteString))
      update = useOS (readProcessQE (proc "apt-get" ["update"]) L.empty)
      aptinstall :: m (Either SomeException (ExitCode, ByteString, ByteString))
      aptinstall = useOS (readProcessVE (proc "apt-get" ["-f", "--yes", "install"]) L.empty)
      configure :: m (Either SomeException (ExitCode, ByteString, ByteString))
      configure = useOS (readProcessVE (proc "dpkg" ["--configure", "-a"]) L.empty)
      upgrade :: m (Either SomeException (ExitCode, ByteString, ByteString))
      upgrade = useOS (readProcessQE (proc "apt-get" ["-f", "-y", "--force-yes", "dist-upgrade"]) L.empty)

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadMask m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- view (osRoot . rootPath) <$> getOS
       withProcAndSys root $ liftIO $ useEnv root (return . force) $ do
         _ <- readProcessV p L.empty
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = ppShow name
      formatPackage (name, Just version) = ppShow name ++ "=" ++ show (prettyDebianVersion version)

#if 0
-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
#endif

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: (MonadIO m, MonadOS m, MonadMask m) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {_envRoot = view osRoot os, _envPath = "/work/localpool"}) (view osLocalMaster os)
       putOS (set osLocalCopy repo' os)
       _ <- updateLists
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache

osFlushPackageCache :: MonadOS m => m ()
osFlushPackageCache = modifyOS (\ os -> os {_osSourcePackageCache = Nothing, _osBinaryPackageCache = Nothing})

-- | Get the version of the newest ghc available in a build environment.
-- ghcNewestAvailableVersion :: (MonadIO m, Functor m, MonadState OSImage m) => m (Maybe DebianVersion)
-- ghcNewestAvailableVersion = do
--   root <- rootPath . osRoot <$> get
--   liftIO $ GHC.ghcNewestAvailableVersion root

buildEssential :: (MonadOS m, MonadIO m) => m Relations
buildEssential = getOS >>= liftIO . OS.buildEssential

syncOS :: (MonadOS m, MonadTop r m, MonadRepos m) => EnvRoot -> m ()
syncOS dstRoot =
    do srcOS <- getOS
       dstOS <- Debian.Repo.Internal.Repos.syncOS srcOS dstRoot
       putOS dstOS
