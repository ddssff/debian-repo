{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.State.OSImage
    ( buildArchOfOS
    , osBinaryPackages
    , osSourcePackages
    , prepareOS
    , updateOS
    ) where

import Control.DeepSeq (force)
import Control.Exception (IOException, throw)
import Control.Lens (at, to, use, view)
import Control.Monad (when)
import Control.Monad.Catch (catch, MonadMask, try)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.Either (partitionEithers)
import Data.List (isSuffixOf)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Codename (codename)
import Debian.Debianize (EnvSet(cleanOS, dependOS))
import qualified Debian.Debianize (EnvSet(buildOS))
import Debian.Except (HasIOException, liftEIO)
import Debian.Pretty (ppShow, prettyShow)
--import Debian.Process (liftEIO)
import Debian.Relation (BinPkgName)
import Debian.Repo.EnvPath (EnvRoot(EnvRoot), rootPath)
import Debian.Repo.IO (buildArchOfRoot)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.MonadRepos (MonadRepos, osImageMap, putOSImage, reposState)
import Debian.Repo.OSImage (createOSImage, OSImage(..), osArch, osBaseDistro, osLocalMaster,
                            osRoot, osSourcePackageCache, osBinaryPackageCache,
                            pbuilder, debootstrap, localeGen, neuterEnv, osFullDistro)
import Debian.Repo.OSKey ({-HasOSKey,-} OSKey(OSKey, _root))
import Debian.Repo.MonadOS (MonadOS, getOS, modifyOS, evalMonadOS, evalMonadOS', aptGetInstall, syncLocalPool, syncOS)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (replaceFile)
import Debian.Repo.Prelude.Process (runV2)
import Debian.Repo.Prelude.SSH (sshCopy)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, quieter, qPutStrLn)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.Slice (NamedSliceList(sliceListName), Slice(sliceSource), SliceList(slices), SourcesChangedAction(SourcesChangedError), UpdateError(..))
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (verifySourcesList)
import Debian.Repo.Top (toTop, distDir, MonadTop, sourcesPath, TopDir(TopDir))
import Debian.Sources (DebSource(_sourceUri), parseSourcesList, sourceUri)
import Debian.TH (here)
import Debian.URI (URI(uriScheme), uriSchemeLens)
import Debian.VendorURI (vendorURI)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode, shell)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)

buildArchOfOS :: (MonadIO m, MonadOS r s m, HasIOException e, MonadError e m) => m Arch
buildArchOfOS = do
  root <- view (osRoot . to _root . rootPath) <$> getOS
  liftIO $ do
    setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
    a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
    b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
    case (code1, lines out1, code2, lines out2) of
      (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
          return $ Binary (ArchOS os) (ArchCPU cpu)
      _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)

osSourcePackages :: forall r s e m. (MonadIO m, MonadRepos s m, MonadOS r s m, HasIOException e, MonadError e m) => m [SourcePackage]
osSourcePackages = do
  mpkgs <- view osSourcePackageCache <$> getOS
  maybe osSourcePackages' return mpkgs
    where
      osSourcePackages' = do
        root <- view (osRoot . to _root) <$> getOS
        arch <- view osArch <$> getOS
        dist <- osFullDistro <$> getOS
        pkgs <- sourcePackagesFromSources root arch dist
        qPutStrLn ("Read " ++ show (length pkgs) ++ " release source packages")
        modifyOS (\ s -> s {_osSourcePackageCache = Just pkgs})
        return pkgs

osBinaryPackages :: (MonadIO m, MonadRepos s m, MonadOS r s m, HasIOException e, MonadError e m) => m [BinaryPackage]
osBinaryPackages = do
  mpkgs <- view osBinaryPackageCache <$> getOS
  maybe osBinaryPackages' return mpkgs
    where
      osBinaryPackages' = do
        root <- view (osRoot . to _root) <$> getOS
        arch <- view osArch <$> getOS
        dist <- osFullDistro <$> getOS
        pkgs <- binaryPackagesFromSources root arch dist
        qPutStrLn ("Read " ++ show (length pkgs) ++ " release binary packages")
        modifyOS (\ s -> s {_osBinaryPackageCache = Just pkgs})
        return pkgs

-- |Find or create and update an OS image.  Returns paths to clean and
-- depend os images.  This involves several fallback actions - first
-- we get the values from MonadRepos, if anything files we look at the
-- file system and construct the value from that (replacing the value
-- in MonadRepos), and if anything fails from that point forward we
-- remove the value in the file system, rebuild everything, and
-- proceed from there.  If that fails we are out of options.
prepareOS
    :: forall s r e m. (MonadIO m, MonadMask m, Show e, HasIOException e, HasRsyncError e, MonadError e m, MonadRepos s m, MonadTop r m) =>
       EnvSet                   -- ^ The location where image is to be built
    -> NamedSliceList           -- ^ The sources.list of the base distribution
    -> [Slice]                  -- ^ Extra repositories - e.g. personal package archives
    -> LocalRepository           -- ^ The location of the local upload repository
    -> Bool                     -- ^ If true, remove and rebuild the image
    -> Bool                     -- ^ If true, flush all the build dependencies
    -> SourcesChangedAction     -- ^ What to do if called with a sources.list that
                                -- differs from the previous call
    -> [BinPkgName]             -- ^ Extra packages to install - e.g. keyrings, software-properties-common
    -> [BinPkgName]             -- ^ More packages to install, but these may not be available
                                -- immediately - e.g seereason-keyring.  Ignore exceptions.
    -> [BinPkgName]             -- ^ Packages to exclude
    -> [String]                 -- ^ Components of the base repository
    -> m (OSKey, OSKey)
prepareOS eset distro extra repo flushRoot flushDepends ifSourcesChanged include optional exclude components =
    do mcleanOS <- use (reposState . osImageMap . at cleanRoot)
       qPutStrLn (prettyShow $here <> " - mcleanOS=" ++ show mcleanOS)
       _cleanOS <- case mcleanOS of
                     Just bar -> return bar
                     Nothing -> do
                       os <- createOSImage cleanRoot distro extra repo
                       putOSImage os
                       return os
       case flushRoot of
         True -> evalMonadOS (recreate (Just Flushed) >> syncOS dependRoot >> syncOS buildRoot) cleanRoot
         False -> do result <- try (evalMonadOS updateOS cleanRoot)
                     case result of
                       Right _ -> return ()
                       Left e -> evalMonadOS (recreate (Just e) >> syncOS dependRoot >> syncOS buildRoot) cleanRoot
       evalMonadOS (doInclude >> doLocales) cleanRoot
       when flushDepends (evalMonadOS (syncOS dependRoot >> syncOS buildRoot) cleanRoot)
       -- Try running a command in the depend environment, if it fails
       -- sync dependOS from cleanOS.
       dependOS' <- use (reposState . osImageMap . at dependRoot)
       case dependOS' of
         Nothing -> do (arch :: Either IOException Arch) <- (liftIO $ try $ useEnv (view (to _root . rootPath) dependRoot) return buildArchOfRoot)
                       case arch of
                         Left _ -> evalMonadOS (syncOS dependRoot) cleanRoot
                         Right _ ->
                             do -- ePutStrLn "createOSImage dependRoot?  I don't understand why this would be done."
                                os <- createOSImage dependRoot distro extra repo
                                putOSImage os
         _ -> pure ()
       evalMonadOS (doIncludeOpt >> doLocales >> syncLocalPool) dependRoot
       return (cleanRoot, dependRoot)
    where
      cleanRoot = OSKey (EnvRoot (cleanOS eset))
      dependRoot = OSKey (EnvRoot (dependOS eset))
      buildRoot = OSKey (EnvRoot (Debian.Debianize.buildOS eset))
      recreate :: forall r' s' e' m'. (MonadIO m', MonadMask m', Show e', HasIOException e', HasRsyncError e', MonadError e' m', MonadOS r' s' m', MonadTop r' m') => Maybe UpdateError -> m' ()
      recreate e =
          case e of
            Just (Changed name path computed installed)
                | ifSourcesChanged == SourcesChangedError ->
                    error $ "FATAL: Sources for " ++ codename name ++ " in " ++ path ++
                            " don't match computed configuration.\n\ncomputed:\n" ++
                            prettyShow computed ++ "\ninstalled:\n" ++
                            prettyShow installed
            _reason -> do
              base <- view osBaseDistro <$> getOS
              sources <- sourcesPath (sliceListName base)
              dist <- distDir (sliceListName base)
              liftIO $ do ePutStrLn $ "Removing and recreating build environment at " ++ ppShow (_root cleanRoot) ++ ": " ++ show e
                          -- ePutStrLn ("removeRecursiveSafely " ++ cleanRoot))
                          mapM_ (removeRecursiveSafely . view (to _root . rootPath)) [cleanRoot, dependRoot, buildRoot]
                          -- ePutStrLn ("createDirectoryIfMissing True " ++ show dist)
                          createDirectoryIfMissing True dist
                          -- ePutStrLn ("writeFile " ++ show sources ++ " " ++ show (show . osBaseDistro $ os))
                          replaceFile sources (prettyShow base)
              rebuildOS cleanRoot distro extra include exclude components

      doInclude :: forall r' s' e' m'. (MonadIO m', MonadMask m', MonadOS r' s' m', HasIOException e', MonadError e' m') => m' ()
      doInclude = aptGetInstall (map (\s -> (s, Nothing)) include)
      doIncludeOpt :: forall r' s' e' m'. (MonadIO m', MonadMask m', MonadOS r' s' m', HasIOException e', MonadError e' m') => m' ()
      doIncludeOpt = aptGetInstall (map (\s -> (s, Nothing)) optional)
                     `catch` (\ (e :: IOError) -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales :: forall r' s' e' m'. (MonadIO m', MonadMask m', MonadOS r' s' m', Show e', HasIOException e', MonadError e' m') => m' ()
      doLocales =
          do os <- getOS
             localeName <- liftIO $ try (getEnv "LANG")
             localeGen os (either (\ (_ :: IOError) -> "en_US.UTF-8") id localeName)

-- | Not used, but could be a substitute for buildOS.
_pbuilderBuild :: (MonadIO m, MonadMask m, Show e, HasRsyncError e, HasIOException e, MonadError e m, MonadRepos s m, MonadTop r m) =>
            OSKey
         -> NamedSliceList
         -> [Slice]
         -> LocalRepository
         -> m OSImage
_pbuilderBuild root distro extra repo =
    do (TopDir top) <- view toTop
       os <- pbuilder top root distro extra repo
       putOSImage os
       try (evalMonadOS updateOS root) >>= either (\ (e :: IOException) -> error (show e)) return
       return os

rebuildOS :: (MonadIO m, MonadMask m, Show e, HasIOException e, HasRsyncError e, MonadError e m, MonadOS r s m) =>
             OSKey                      -- ^ The location where image is to be built
           -> NamedSliceList            -- ^ The sources.list of the base distribution
           -> [Slice]
           -> [BinPkgName]              -- ^ Extra packages to install - e.g. keyrings
           -> [BinPkgName]              -- ^ Packages to exclude
           -> [String]                  -- ^ Components of the base repository
           -> m ()
rebuildOS root distro extra include exclude components =
          do master <- view osLocalMaster <$> getOS
             _key <- buildOS root distro extra master include exclude components
             syncLocalPool

-- | Create a new clean build environment in root.clean FIXME: create
-- an ".incomplete" flag and remove it when build-env succeeds
buildOS :: (MonadIO m, MonadMask m, Show e, HasIOException e, HasRsyncError e, MonadError e m, MonadRepos s m) =>
            OSKey
         -> NamedSliceList
         -> [Slice]
         -> LocalRepository
         -> [BinPkgName]
         -> [BinPkgName]
         -> [String]
         -> m OSImage
buildOS root distro extra repo include exclude components =
    do os <- debootstrap root distro extra repo include exclude components
       putOSImage os
       evalMonadOS' updateOS root
       liftEIO [$here] $ neuterEnv os
       return os

-- | Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateOS :: (MonadIO m, MonadMask m, Show e, HasIOException e, HasRsyncError e, MonadError e m, MonadOS r s m) => m ()
updateOS = quieter 1 $ do
  rpath <- view (osRoot . to _root . rootPath) <$> getOS
  liftEIO [$here] $ createDirectoryIfMissing True (rpath </> "etc")
  liftEIO [$here] $ readFile "/etc/resolv.conf" >>= writeFile (rpath </> "etc/resolv.conf")
  liftEIO [$here] $ prepareDevs rpath
  syncLocalPool
  ePutStrLn ("updateOS - " ++ prettyShow $here)
  verifySources
  ePutStrLn ("updateOS - " ++ prettyShow $here)
  -- Disable the starting of services in the changeroot
  _ <- liftEIO [$here] $ useEnv rpath (return . force) $ readProcessWithExitCode "dpkg-divert" ["--local", "--rename", "--add", "/sbin/initctl"] ""
  ePutStrLn ("updateOS - " ++ prettyShow $here)
  _ <- liftEIO [$here] $ useEnv rpath (return . force) $ readProcessWithExitCode "ln" ["-s", "/bin/true", "/sbin/initctl"] ""
  ePutStrLn ("updateOS - " ++ prettyShow $here)
  code <- liftEIO [$here] $ sshCopy rpath
  ePutStrLn ("updateOS - " ++ prettyShow $here)
  case code of
    ExitSuccess -> return ()
    _ -> error $ "sshCopy -> " ++ show code
    where
      -- verifySources :: (MonadIO m, MonadOS r s m, MonadError e m) => m ()
      verifySources =
          do root <- view (osRoot . to _root) <$> getOS
             ePutStrLn ("updateOS - root=" ++ show root ++ " at " ++ prettyShow $here)
             computed <- remoteOnly <$> osFullDistro <$> getOS
             ePutStrLn ("updateOS - computed=" ++ show computed ++ " at " ++ prettyShow $here)
             let sourcesPath' = (view rootPath root </> "etc/apt/sources.list")
             ePutStrLn ("updateOS - sourcesPath'=" ++ show sourcesPath' ++ " at " ++ prettyShow $here)
             let sourcesD = (view rootPath root </> "etc/apt/sources.list.d")
             ePutStrLn ("updateOS - sourcesD=" ++ show sourcesD ++ " at " ++ prettyShow $here)
             sourcesDPaths <- liftIO $ (map (sourcesD </>) . filter (isSuffixOf ".list")) <$> getDirectoryContents sourcesD
             ePutStrLn ("updateOS - sourcesDPaths=" ++ show sourcesDPaths ++ " at " ++ prettyShow $here)
             (errors, sourcesText) <- liftIO $ partitionEithers <$> mapM (try . readFile) (sourcesPath' : sourcesDPaths)
             ePutStrLn ("updateOS - sourcesText=" ++ show sourcesText ++ " at " ++ prettyShow $here)
             -- text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case (errors :: [IOException]) of
                   [] -> Just <$> verifySourcesList [$here] (Just root) (remoteOnly' (parseSourcesList [$here] (unlines sourcesText)))
                   exns -> error $  unlines (("Failure verifying sources in " ++ show root ++ ":") : map show exns)
{-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
-}
             case installed of
               Nothing -> (view osBaseDistro <$> getOS) >>= \ sources -> throw $ Missing (sliceListName sources) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       (view osBaseDistro <$> getOS) >>= \ sources -> throw $ Changed (sliceListName sources) sourcesPath' computed installed'
               _ -> return ()
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . view vendorURI . _sourceUri . sliceSource $ y) /= "file:"
      remoteOnly' :: [DebSource] -> [DebSource]
      remoteOnly' xs = filter (\x -> view (sourceUri . vendorURI . uriSchemeLens) x /= "file:") xs
{-
    get >>= updateOS' >>= either throw put
    where
      updateOS' :: MonadRepos s m => OSImage -> m (Either UpdateError OSImage)
      updateOS' os =
          do let root = getL osRoot os
             liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc")
             liftIO $ readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
             verified <- verifySources os
             case verified of
               Left x -> return $ Left x
               Right _ ->
                   do liftIO $ prepareDevs (rootPath root)
                      evalMonadOS (syncLocalPool >> updateLists) os
                      _ <- liftIO $ sshCopy (rootPath root)
                      source' <- evalMonadOS getSourcePackages' os'
                      binary <- evalMonadOS getBinaryPackages' os'
                      return . Right $ setL osSourcePackages source' $ setL osBinaryPackages binary $ os'
      verifySources :: MonadRepos s m => OSImage -> m (Either UpdateError OSImage)
      verifySources os =
          do let root = getL osRoot os
             computed <- remoteOnly <$> evalMonadOS osFullDistro [SourceOption "trusted" OpSet ["yes"]] os
             let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (sliceListName (getL osBaseDistro os)) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       return $ Left $ Changed (sliceListName (getL osBaseDistro os)) sourcesPath' computed installed'
               _ -> return $ Right os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . _sourceUri . sliceSource $ y) /= "file:"
-}

-- |Prepare a minimal \/dev directory
{-# WARNING prepareDevs "This function should check all the result codes" #-}
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev devices
  where
    devices :: [(FilePath, String, Int, Int)]
    devices = [(root ++ "/dev/null", "c", 1, 3),
               (root ++ "/dev/zero", "c", 1, 5),
               (root ++ "/dev/full", "c", 1, 7),
               (root ++ "/dev/console", "c", 5, 1),
               (root ++ "/dev/random", "c", 1, 8),
               (root ++ "/dev/urandom", "c", 1, 9)] ++
              (map (\ n -> (root ++ "/dev/loop" ++ show n, "b", 7, n)) [0..7]) ++
              (map (\ n -> (root ++ "/dev/loop/" ++ show n, "b", 7, n)) [0..7])
    prepareDev :: (FilePath, String, Int, Int) -> IO (Either IOException (ExitCode, L.ByteString, L.ByteString))
    prepareDev (path, typ, major, minor) = do
                     createDirectoryIfMissing True (fst (splitFileName path))
                     let cmd = "mknod " ++ path ++ " " ++ typ ++ " " ++ show major ++ " " ++ show minor ++ " 2> /dev/null"
                     exists <- doesFileExist path
                     case exists of
                       False -> try (runV2 [$here] (shell cmd) L.empty)
                       True -> return $ Right (ExitSuccess, mempty, mempty)
