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

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (evaluate, SomeException)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask, throwM, try)
import Control.Monad.State (MonadState(get), StateT, evalStateT, get)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Data.Monoid ((<>))
import Data.Time (NominalDiffTime)
import Debian.Pretty (pretty)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(rootPath))
import Debian.Repo.Internal.Repos (MonadRepos, osFromRoot, putOSImage, syncOS)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.OSImage as OS (OSImage(osRoot, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache))
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude (readProc, runProc, symbol)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((</>))
import System.Process (proc, readProcess)
import System.Process.Progress (keepResult, qPutStrLn, quieter, timeTask)
import System.Unix.Chroot (useEnv)

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

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadRepos m => StateT EnvRoot m a -> EnvRoot -> m a
evalMonadOS task root = do
  a <- evalStateT task root
  return a

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m NominalDiffTime
updateLists = quieter 1 $
    do root <-rootPath . osRoot <$> getOS
       withProc $ liftIO $ do
         qPutStrLn ("Updating OSImage " ++ root)
         out <- useEnv root forceList (readProc update)
         _ <- case keepResult out of
                [ExitFailure _] ->
                    do _ <- useEnv root forceList (runProc configure)
                       useEnv root forceList (runProc update)
                _ -> return []
         (_, elapsed) <- timeTask (useEnv root forceList (runProc upgrade))
         return elapsed
    where
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

-- | Do an IO task in the build environment with /proc mounted.
withProc :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withProc task =
    do root <- rootPath . osRoot <$> getOS
       let proc = root </> "proc"
           sys = root </> "sys"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True proc >> readProcess "mount" ["--bind", "/proc", proc] "" >>
                         createDirectoryIfMissing True sys >> readProcess "mount" ["--bind", "/sys", sys] "")
           post :: String -> m String
           post _s = liftIO $ readProcess "umount" [proc] "" >> readProcess "umount" [sys] ""
           task' :: String -> m c
           task' _s = task
       bracket pre post task'

-- | Do an IO task in the build environment with /proc mounted.
withTmp :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withTmp task =
    do root <- rootPath . osRoot <$> getOS
       let dir = root </> "tmp"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/tmp", dir] "")
           post :: String -> m String
           post _ = liftIO $ readProcess "umount" [dir] ""
           task' :: String -> m c
           task' _ = try task >>= either (\ (e :: SomeException) -> throwM e) return
       bracket pre post task'

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- rootPath . osRoot <$> getOS
       liftIO $ useEnv root (return . force) $ do
         _ <- runProc p
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: (MonadIO m, MonadOS m) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {envRoot = osRoot os, envPath = "/work/localpool"}) (osLocalMaster os)
       putOS (os {osLocalCopy = repo'})
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
