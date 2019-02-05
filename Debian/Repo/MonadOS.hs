{-# LANGUAGE ConstraintKinds, CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS, getOS, putOS, modifyOS
    , evalMonadOS, evalMonadOS'
    , updateLists
    , aptGetInstall
    , syncLocalPool
    , osFlushPackageCache
    , buildEssential
    , Debian.Repo.MonadOS.syncOS
    , runV, runVE, runQ, runQE
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, pure, (<$>))
#endif
import Control.DeepSeq (force)
import Control.Exception ({-evaluate,-} SomeException)
import Control.Lens (at, set, to, use, view)
import Control.Monad.Catch (mask_, try)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Except () -- instances
import Data.ByteString.Lazy as L (ByteString, empty)
import Data.String (IsString)
import Data.Traversable
import Debian.Pretty (ppShow)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(..), rootPath)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.MonadRepos (MonadRepos, osImageMap, putOSImage, reposState, syncOS)
import Debian.Repo.Mount (withProcAndSys)
import Debian.Repo.OSImage as OS (OSImage(..), osRoot, osLocalMaster, osLocalCopy)
import Debian.Repo.OSKey (OSKey(..), HasOSKey(..))
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude.Process (CreateProcess, run, RunOptions(..), showCommandAndResult, putIndented)
import Debian.Repo.Prelude.Verbosity (ePutStrLn)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Prelude hiding (mapM, sequence)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (proc)
import System.Process.ListLike (ListLikeProcessIO, showCreateProcessForUser)
import System.Unix.Chroot (useEnv)
-- import System.Unix.Mount (withProcAndSys, WithProcAndSys)

-- | The problem with having an OSImage in the state of MonadOS is
-- that then we are modifying a copy of the OSImage in MonadRepos, we
-- want to go into MonadRepos and modify the map element there.  So
-- instead put an EnvRoot to look up the OSImage.
type MonadOS r s m = (HasOSKey r, MonadReader r m, MonadRepos s m)

getOS :: MonadOS r s m => m OSImage
getOS = view osKey >>= \key -> maybe (error "getOS") id <$> use (reposState . osImageMap . at key)

putOS :: MonadOS r s m => OSImage -> m ()
putOS = putOSImage

modifyOS :: MonadOS r s m => (OSImage -> OSImage) -> m ()
modifyOS f = getOS >>= putOS . f

-- | Perform a task in the changeroot of an OS.
useOS :: MonadOS r s m => m a -> m a
useOS action =
  do root <- view (osRoot . to _root . rootPath) <$> getOS
     withProcAndSys root $ useEnv root (return {-. force-}) action

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadReader r m => ReaderT (r, OSKey) m a -> OSKey -> m a
evalMonadOS task key = ask >>= \r -> runReaderT task (r, key)

evalMonadOS' :: ReaderT OSKey m a -> OSKey -> m a
evalMonadOS' task root = runReaderT task root

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: forall r s m. (Applicative m, MonadOS r s m) => m Bool
updateLists = do
  r1 <- update >>= f
  r2 <- if r1 then pure True else or <$> sequence [aptinstall >>= f, configure >>= f, update >>= f]
  if r2 then mask_ upgrade >>= f else pure False
    where
      f :: (Either SomeException (ExitCode, ByteString, ByteString)) -> m Bool
      f (Right (ExitSuccess, _, _)) = return True
      f _ = return False
      update :: m (Either SomeException (ExitCode, ByteString, ByteString))
      update = try $ useOS (runQ (proc "apt-get" ["update"]) L.empty)
      aptinstall :: m (Either SomeException (ExitCode, ByteString, ByteString))
      aptinstall = try $ useOS (runV (proc "apt-get" ["-f", "--yes", "install"]) L.empty)
      configure :: m (Either SomeException (ExitCode, ByteString, ByteString))
      configure = try $ useOS (runV (proc "dpkg" ["--configure", "-a"]) L.empty)
      upgrade :: m (Either SomeException (ExitCode, ByteString, ByteString))
      upgrade = try $ useOS (runQ (proc "apt-get" ["-f", "-y", "--force-yes", "dist-upgrade"]) L.empty)

runV :: (Eq c, IsString a, ListLikeProcessIO a c, MonadOS r s m) => CreateProcess -> a -> m (ExitCode, a, a)
runV p input =
    run (StartMessage showCommand' <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

runVE :: (Eq c, IsString a, ListLikeProcessIO a c, MonadOS r s m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runVE p input = try $ runV p input

runQ :: (ListLikeProcessIO a c, MonadOS r s m) => CreateProcess -> a -> m (ExitCode, a, a)
runQ p input =
    run (StartMessage showCommand' <> FinishMessage showCommandAndResult) p input

runQE :: (ListLikeProcessIO a c, MonadOS r s m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runQE p input = try $ runQ p input

showCommand' :: MonadOS r s m => String -> CreateProcess -> m ()
showCommand' prefix p = view osKey >>= \key -> ePutStrLn (prefix ++ showCreateProcessForUser p ++ " (in " ++ show key ++ ")")

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS r s m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- view (osRoot . to _root . rootPath) <$> getOS
       withProcAndSys root $ useEnv root (return . force) $ do
         _ <- runV p L.empty
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
syncLocalPool :: (MonadIO m, MonadOS r s m) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {_envRoot = view (osRoot . to _root) os, _envPath = "/work/localpool"}) (view osLocalMaster os)
       putOS (set osLocalCopy repo' os)
       _ <- updateLists
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache

osFlushPackageCache :: MonadOS r s m => m ()
osFlushPackageCache = modifyOS (\ os -> os {_osSourcePackageCache = Nothing, _osBinaryPackageCache = Nothing})

-- | Get the version of the newest ghc available in a build environment.
-- ghcNewestAvailableVersion :: (MonadIO m, Functor m, MonadState OSImage m) => m (Maybe DebianVersion)
-- ghcNewestAvailableVersion = do
--   root <- rootPath . osRoot <$> get
--   liftIO $ GHC.ghcNewestAvailableVersion root

buildEssential :: MonadOS r s m => m Relations
buildEssential = getOS >>= liftIO . OS.buildEssential

syncOS :: (MonadOS r s m, MonadTop r m) => OSKey -> m ()
syncOS dstRoot =
    do srcOS <- getOS
       dstOS <- Debian.Repo.MonadRepos.syncOS srcOS dstRoot
       putOS dstOS
