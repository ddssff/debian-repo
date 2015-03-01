{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS(getOS, putOS, modifyOS)
    , evalMonadOS
    , updateLists
    , withProc
    , withTmp
    , aptGetInstall
    , syncLocalPool
    , osFlushPackageCache
    , buildEssential
    , Debian.Repo.MonadOS.syncOS
    ) where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate, SomeException, catch)
import Control.Monad (MonadPlus, liftM, msum, mzero)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask, throwM, try)
import Control.Monad.State (MonadState(get), StateT, evalStateT, get)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Control.Monad.Trans.Except () -- instances
import Data.ByteString.Lazy as L (ByteString, empty)
import Data.Monoid (Monoid, mappend)
import Data.Time (NominalDiffTime)
import Data.Traversable
import Debian.Pretty (ppShow)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(rootPath))
import Debian.Repo.Internal.Repos (MonadRepos, osFromRoot, putOSImage, syncOS)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.OSImage as OS (OSImage(osRoot, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache))
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude.Process (timeTask, readProcessVE, readProcessV, readProcessQE)
import Debian.Repo.Prelude.Verbosity (quieter, ePutStrLn)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Prelude hiding (mapM, sequence)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (proc)
import System.Unix.Chroot (useEnv)

instance NFData SomeException

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

useOS :: (MonadOS m, MonadIO m, MonadMask m, NFData a) => IO a -> m a
useOS action =
  do root <- rootPath . osRoot <$> getOS
     withProc $ liftIO $ useEnv root (return . force) action

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadRepos m => StateT EnvRoot m a -> EnvRoot -> m a
evalMonadOS task root = do
  a <- evalStateT task root
  return a

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: forall m. (Applicative m, MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m Bool
updateLists = do
  r1 <- update >>= f
  r2 <- if r1 then pure True else or <$> sequence [aptinstall >>= f, configure >>= f, update >>= f]
  if r2 then upgrade >>= f else pure False
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
      upgrade = useOS (readProcessVE (proc "apt-get" ["-f", "-y", "--force-yes", "dist-upgrade"]) L.empty)

-- | Do an IO task in the build environment with /proc mounted.
withProc :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withProc task =
    do root <- rootPath . osRoot <$> getOS
       let proc' = root </> "proc"
           sys = root </> "sys"
           pre :: m ()
           mountProc = proc "mount" ["--bind", "/proc", proc']
           mountSys = proc "mount" ["--bind", "/sys", sys]
           umountProc = proc "umount" [proc']
           umountSys = proc "umount" [sys]
           umountProcLazy = proc "umount" ["-l", proc']
           umountSysLazy = proc "umount" ["-l", sys]

           pre = liftIO (do createDirectoryIfMissing True proc'
                            readProcessV mountProc L.empty
                            createDirectoryIfMissing True sys
                            readProcessV mountSys L.empty
                            return ())
           post :: () -> m ()
           post _ = liftIO $ do readProcessV umountProc L.empty
                                  `catch` (\ (e :: IOError) ->
                                               ePutStrLn ("Exception unmounting proc, trying lazy: " ++ show e) >>
                                               readProcessV umountProcLazy L.empty)
                                readProcessV umountSys L.empty
                                  `catch` (\ (e :: IOError) ->
                                               ePutStrLn ("Exception unmounting sys, trying lazy: " ++ show e) >>
                                               readProcessV umountSysLazy L.empty)
                                return ()
           task' :: () -> m c
           task' _ = task
       bracket pre post task'

-- | Do an IO task in the build environment with /proc mounted.
withTmp :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withTmp task =
    do root <- rootPath . osRoot <$> getOS
       let dir = root </> "tmp"
           mountTmp = proc "mount" ["--bind", "/tmp", dir]
           umountTmp = proc "umount" [dir]
           pre :: m ()
           pre = liftIO $ do createDirectoryIfMissing True dir
                             readProcessV mountTmp L.empty
                             return ()
           post :: () -> m ()
           post _ = liftIO $ readProcessV umountTmp L.empty >> return ()
           task' :: () -> m c
           task' _ = try task >>= either (\ (e :: SomeException) -> throwM e) return
       bracket pre post task'

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- rootPath . osRoot <$> getOS
       liftIO $ useEnv root (return . force) $ do
         readProcessV p L.empty
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = ppShow name
      formatPackage (name, Just version) = ppShow name ++ "=" ++ show (prettyDebianVersion version)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: (Applicative m, MonadIO m, MonadOS m, MonadCatch m, MonadMask m) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {envRoot = osRoot os, envPath = "/work/localpool"}) (osLocalMaster os)
       putOS (os {osLocalCopy = repo'})
       updateLists
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache

osFlushPackageCache :: MonadOS m => m ()
osFlushPackageCache = modifyOS (\ os -> os {osSourcePackageCache = Nothing, osBinaryPackageCache = Nothing})

-- | Get the version of the newest ghc available in a build environment.
-- ghcNewestAvailableVersion :: (MonadIO m, Functor m, MonadState OSImage m) => m (Maybe DebianVersion)
-- ghcNewestAvailableVersion = do
--   root <- rootPath . osRoot <$> get
--   liftIO $ GHC.ghcNewestAvailableVersion root

buildEssential :: (MonadOS m, MonadIO m) => m Relations
buildEssential = getOS >>= liftIO . OS.buildEssential

syncOS :: (MonadOS m, MonadTop m, MonadRepos m) => EnvRoot -> m ()
syncOS dstRoot =
    do srcOS <- getOS
       dstOS <- Debian.Repo.Internal.Repos.syncOS srcOS dstRoot
       putOS dstOS
