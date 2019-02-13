{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.OSImage
    (
    -- * OSImage type
      OSImage(..)
    , osRoot, osBaseDistro, osArch, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache
    , createOSImage
    , cloneOSImage

    -- * OSImage Creation
    , pbuilder
    , debootstrap
    , syncOS'

    -- * OSImage Queries
    , osFullDistro
    , buildEssential

    -- * OSImage Manipulation
    , neuterEnv
    , restoreEnv
    , localeGen
    , removeEnv
    ) where

-- import Control.DeepSeq (force)
import Control.Exception (IOException, SomeException)
import Control.Lens (makeLenses, to, view)
import Control.Monad.Catch (MonadCatch, try, MonadMask)
import Control.Monad.Except (catchError, ExceptT, liftEither, liftIO, MonadError, MonadIO, runExceptT, withExceptT)
--import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Data (Data)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Debian.Arch (Arch)
import Debian.Codename (Codename, codename, parseCodename)
import Debian.Except (HasIOException, liftEIO)
import Debian.Pretty (prettyShow)
import Debian.Relation (BinPkgName(..), ParseRelations(parseRelations), Relations)
import Debian.Release (parseSection')
import Debian.Repo.EnvPath (EnvPath(..), rootPath, outsidePath)
import Debian.Repo.IO (buildArchOfRoot)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.OSKey ({-HasOSKey,-} OSKey(..))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (isSublistOf, replaceFile, sameInode)
import Debian.Repo.Prelude.Process (runV2)
import Debian.Repo.Prelude.Verbosity (qPutStr, qPutStrLn, ePutStr, ePutStrLn)
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Rsync (HasRsyncError(fromRsyncError), RsyncError, rsyncOld)
import Debian.Repo.Slice (NamedSliceList(sliceList), NamedSliceList(sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList(..), addAptRepository)
import Debian.Sources (DebSource(..), sourceDist, sourceUri, SourceOption(..), SourceType(..), SourceOp(..))
import Debian.TH (here)
import Debian.URI (uriToString')
import Debian.VendorURI (vendorURI)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Files (createLink, deviceID, fileID, FileStatus, modificationTime)
import System.Process (shell)
import System.Process.ByteString ()
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.Regex (matchRegex, mkRegex)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { _osRoot :: OSKey
         , _osBaseDistro :: NamedSliceList
         , _osArch :: Arch
         , _osExtraRepos :: [Slice]
         , _osLocalMaster :: LocalRepository
         -- ^ The associated local repository, where packages we build
         -- inside this image are first uploaded to.
         , _osLocalCopy :: LocalRepository
         -- ^ A copy of osLocalMaster located inside the os root environment.
         , _osSourcePackageCache :: Maybe [SourcePackage]
         , _osBinaryPackageCache :: Maybe [BinaryPackage]
         }

$(makeLenses ''OSImage)

instance Ord OSImage where
    compare a b = case compare (view osRoot a) (view osRoot b) of
                    EQ -> case compare (view osBaseDistro a) (view osBaseDistro b) of
                            EQ -> compare (view osArch a) (view osArch b)
                            x -> x
                    x -> x

instance Eq OSImage where
    a == b = compare a b == EQ

-- |Create an OS image record
createOSImage ::
    forall e m. (MonadIO m, MonadCatch m,
                 {-HasOSKey r, MonadReader r m,-}
                 HasRsyncError e, HasIOException e, MonadError e m)
    => OSKey                     -- ^ The location where image is to be built
    -> NamedSliceList            -- ^ The sources.list of the base distribution
    -> [Slice]
    -> LocalRepository           -- ^ The location of the local upload repository
    -> m OSImage
createOSImage (OSKey root) distro extra repo =
    do copy <- action'
       -- At this point we can only support the build architecture of
       -- the underlying system.  We can support multiple
       -- distributions, but if the hardware is an amd64 the packages
       -- produced will be amd64.
       arch <- liftEIO [$here] buildArchOfRoot
       let os = OS { _osRoot = OSKey root
                   , _osBaseDistro = distro
                   , _osArch = arch
                   , _osExtraRepos = extra
                   , _osLocalMaster = repo
                   , _osLocalCopy = copy
                   , _osSourcePackageCache = Nothing
                   , _osBinaryPackageCache = Nothing }
       return os
    where
      action' :: m LocalRepository
      action' = liftEither =<< runExceptT (withExceptT fromRsyncError action)
      action :: forall m'. (MonadIO m', MonadCatch m') => ExceptT RsyncError m' LocalRepository
      action = copyLocalRepo (EnvPath {_envRoot = root, _envPath = "/work/localpool"}) repo

-- | Create the OSImage record for a copy of an existing OSImage at a
-- different location.
cloneOSImage ::
    forall e m. (MonadIO m, MonadCatch m,
                 --HasOSKey r, MonadReader r m,
                 HasRsyncError e, HasIOException e, MonadError e m)
    => OSImage -> OSKey -> m OSImage
cloneOSImage src dst = do
  copy <- action'
  return $ src {_osRoot = dst, _osLocalCopy = copy}
    where
      action' :: m LocalRepository
      action' = liftEither =<< runExceptT (withExceptT id action)
      action :: forall m'. (MonadIO m', MonadCatch m'{-, MonadReader r m'-}) => ExceptT e m' LocalRepository
      action = copyLocalRepo (EnvPath {_envRoot = _root dst, _envPath = "/work/localpool"}) (view osLocalMaster src)

-- | Set the location of the OSImage's root directory - where you
-- would cd to before running chroot.
-- chrootEnv :: OSImage -> EnvRoot -> OSImage
-- chrootEnv os dst = os {osRoot = dst}

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               view (osRoot . to _root . rootPath) os,
                               codename (sliceListName (view osBaseDistro os)),
                               show (view osArch os),
                               show (view osLocalCopy os)]

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os =
    let base = view osBaseDistro os
        repo' = view osLocalCopy os
        name = codename (sliceListName base)
        localSources :: SliceList
        localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey repo', sliceSource = src},
                                            Slice {sliceRepoKey = repoKey repo', sliceSource = bin}]}
        src = DebSource Deb [SourceOption "trusted" OpSet ["yes"]] (repoURI repo') (Right (parseCodename name, [parseSection' "main"]))
        bin = DebSource DebSrc [SourceOption "trusted" OpSet ["yes"]] (repoURI repo') (Right (parseCodename name, [parseSection' "main"])) in
    SliceList { slices = slices (sliceList base) ++ view osExtraRepos os ++ slices localSources }

data UpdateError
    = Changed Codename FilePath SliceList SliceList
    | Missing Codename FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, prettyShow l1, prettyShow l2]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

syncOS' ::
    forall e m. (MonadIO m, MonadCatch m,
                 -- HasOSKey r, MonadReader r m,
                 HasRsyncError e, HasIOException e, MonadError e m)
    => OSImage -> OSKey -> m OSImage
syncOS' src dst = do
  liftIO mkdir
  liftIO umount
  (_result, _, _) <- action' -- rsyncOld ["--exclude=/work/build/*"] (view (osRoot . to _root . rootPath) src) (view (to _root . rootPath) dst)
  cloneOSImage src dst
    where
      mkdir = createDirectoryIfMissing True (view (to _root . rootPath) dst ++ "/work")
      umount =
          do srcResult <- umountBelow False (view (osRoot . to _root . rootPath) src)
             dstResult <- umountBelow False (view (to _root . rootPath) dst)
             case filter (\ (_, (code, _, _)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return ()
               failed -> fail $ "umount failure(s): " ++ show failed

      action' :: m (Either IOException ExitCode, String, String)
      action' = liftEither =<< runExceptT (withExceptT fromRsyncError action)
      action :: forall m'. (MonadIO m', MonadCatch m'{-, MonadReader r m'-}) => ExceptT RsyncError m' (Either IOException ExitCode, String, String)
      action = rsyncOld ["--exclude=/work/build/*"] (view (osRoot . to _root . rootPath) src) (view (to _root . rootPath) dst)

-- | FIXME - we should notice the locale problem and run this.
localeGen ::
    forall e m. (MonadIO m, MonadMask m, MonadError e m, Show e)
    => OSImage -> String -> m ()
localeGen os locale =
    do let root = view osRoot os
       qPutStr ("Generating locale " ++  locale ++ " (in " ++ stripDist (view (to _root . rootPath) root) ++ ")...")
       useEnv (view (to _root . rootPath) root) return (runV2 [$here] (shell cmd) B.empty >>= \chunks -> case chunks of
                                                                                                  (ExitSuccess, _, _) -> qPutStrLn "done"
                                                                                                  e -> error $ "Failed to generate locale " ++ view (to _root . rootPath) root ++ ": " ++ cmd ++ " -> " ++ show e)
         `catchError` (\(e :: e) -> error $ "Failed to generate locale " ++ view (to _root . rootPath) root ++ ": " ++ cmd ++ " -> " ++ show e)
    where
      cmd = "locale-gen " ++ locale


-- |To "neuter" an executable is to replace it with a hard link to
-- \/bin\/true in such a way that the operation can be reversed.  This
-- is done in order to make it safe to install files into it when it
-- isn't "live".  If this operation fails it is assumed that the
-- image is damaged, so it is removed.
neuterEnv :: OSImage -> IO ()
neuterEnv os =
    do qPutStr ("Neutering OS image (" ++ stripDist root ++ ")...")
       result <- try $ mapM_ (neuterFile os) neuterFiles
       either (\ (e :: SomeException) -> error $ "Failed to neuter environment " ++ root ++ ": " ++ show e)
              (\ _ -> qPutStrLn "done.")
              result
    where
      root = view (osRoot . to _root . rootPath) os

neuterFiles :: [(FilePath, Bool)]
neuterFiles = [("/sbin/start-stop-daemon", True),
               ("/usr/sbin/invoke-rc.d", True),
               ("/sbin/init",False),
               ("/usr/sbin/policy-rc.d", False)]

-- neuter_file from build-env.ml
neuterFile :: OSImage -> (FilePath, Bool) -> IO ()
neuterFile os (file, mustExist) =
    do
      -- putStrBl ("Neutering file " ++ file)
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          neuterExistantFile else
          if mustExist then
              error ("Can't neuter nonexistant file: " ++ outsidePath fullPath) else
              return () -- putStrBl "File doesn't exist, nothing to do"

    where
      neuterExistantFile =
          do
            sameFile <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            if sameFile then
                return () else -- putStrBl "File already neutered"
                neuterUnneuteredFile
      neuterUnneuteredFile =
          do
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            if hasReal then
                neuterFileWithRealVersion else
                neuterFileWithoutRealVersion
            createLink (outsidePath binTrue) (outsidePath fullPath)
      neuterFileWithRealVersion =
          do
            same <- ((==) `on` md5) <$> L.readFile (outsidePath fullPath) <*> L.readFile (outsidePath fullPath ++ ".real")
            case same of
              True -> removeFile (outsidePath fullPath)
              False -> error (file ++ " and " ++ file ++ ".real differ (in " ++ view rootPath root ++ ")")

      neuterFileWithoutRealVersion = renameFile (outsidePath fullPath) (outsidePath fullPath ++ ".real")

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = view (osRoot . to _root) os

-- |Reverse the neuterEnv operation.
restoreEnv :: OSImage -> IO OSImage
restoreEnv os =
    do
      qPutStr "De-neutering OS image..."
      result <- try $ mapM_ (restoreFile os) neuterFiles
      either (\ (e :: SomeException) -> error $ "damaged environment " ++ view (to _root . rootPath) root ++ ": " ++ show e ++ "\n  please remove it.")
                 (\ _ -> return os) result
    where
      root = view osRoot os

-- check_and_restore from build-env.ml
restoreFile :: OSImage -> (FilePath, Bool) -> IO ()
restoreFile os (file, mustExist) =
    do
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          restoreExistantFile else
          if mustExist then
              error ("Can't restore nonexistant file: " ++ outsidePath fullPath) else
              return ()
    where
      restoreExistantFile =
          do
            isTrue <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            case (isTrue, hasReal) of
              (True, True) ->
                  do
                    removeFile (outsidePath fullPath)
                    renameFile (outsidePath fullPath ++ ".real") (outsidePath fullPath)
              (False, _) -> error "Can't restore file not linked to /bin/true"
              (_, False) -> error "Can't restore file with no .real version"

      fullPath = EnvPath root2 file
      binTrue = EnvPath root2 "/bin/true"
      root2 = view (to _root) root1
      root1 = view osRoot os

-- | Build the dependency relations for the build essential packages.
-- For this to work the @build-essential@ package must be installed in
-- the OSImage.
buildEssential :: OSImage -> IO Relations
buildEssential os = do
      let root = view osRoot os
      -- qPutStrLn "Computing build essentials"
      essential <-
          readFile (view (to _root . rootPath) root ++ "/usr/share/build-essential/essential-packages-list") >>=
          return . lines >>= return . dropWhile (/= "") >>= return . tail >>= return . filter (/= "sysvinit") >>=
          return . parseRelations . (intercalate ", ") >>=
          return . (either (error "parse error in /usr/share/build-essential/essential-packages-list") id)
      let re = mkRegex "^[^ \t]"
      relationText <-
          readFile (view (to _root . rootPath) root ++ "/usr/share/build-essential/list") >>=
          return . lines >>=
          return . dropWhile (/= "BEGIN LIST OF PACKAGES") >>= return . tail >>=
          return . takeWhile (/= "END LIST OF PACKAGES") >>=
          return . filter ((/= Nothing) . (matchRegex re))
      -- ePut ("buildEssentialText: " ++ intercalate ", " relationText)
      let buildEssential'' = parseRelations (intercalate ", " relationText)
      let buildEssential' = either (\ l -> error ("parse error in /usr/share/build-essential/list:\n" ++ show l)) id buildEssential''
      return (essential ++ buildEssential')

-- |Remove an image.  The removeRecursiveSafely function is used to
-- ensure that any file systems mounted inside the image are unmounted
-- and not destroyed.
removeEnv :: OSImage -> IO ()
removeEnv os =
    do
      ePutStr "Removing build environment..."
      removeRecursiveSafely (view (to _root . rootPath) root)
      ePutStrLn "done."
    where
      root = view osRoot os

-- prefixes :: Maybe (L.ByteString, L.ByteString)
-- prefixes = Just (" 1> ", " 2> ")

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)

-- | This is a deepseq thing
-- forceList :: [a] -> IO [a]
-- forceList output = evaluate (length output) >> return output

pbuilder ::
    forall e m. (MonadIO m, MonadCatch m, {-, HasOSKey r, MonadReader r m-}
                 HasRsyncError e, HasIOException e, MonadError e m, Show e)
    => FilePath
    -> OSKey
    -> NamedSliceList
    -> [Slice]
    -> LocalRepository
    -> m OSImage
pbuilder top root distro extra repo =
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
    do ePutStrLn ("Creating clean build environment (" ++ codename (sliceListName distro) ++ ")")
       ePutStrLn ("# " ++ cmd top)
       let codefn (ExitSuccess, _, _) = return ()
           codefn failure = error ("Could not create build environment:\n " ++ cmd top ++ " -> " ++ show failure)
       (runV2 [$here] (shell (cmd top)) B.empty >>= codefn) `catchError` (\(e :: e) -> error ("Could not create build environment:\n " ++ cmd top ++ " -> " ++ show e))
       ePutStrLn "done."
       os <- createOSImage root distro extra repo -- arch?  copy?
       let sourcesPath' = view (to _root . rootPath) root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
           sources = prettyShow $ osFullDistro os
       liftIO $ replaceFile sourcesPath' sources
       _ <- liftIO $ useEnv (view (to _root . rootPath) root) return $ mapM addAptRepository extra
       return os
    where
      cmd x =
          intercalate " " $ [ "pbuilder"
                            , "--create"
                            , "--distribution", (codename . sliceListName $ distro)
                            , "--basetgz", x </> "pbuilderBase"
                            , "--buildplace", view (to _root . rootPath) root
                            , "--preserve-buildplace"
                            ]

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
debootstrap
    :: forall e m. (MonadIO m, MonadCatch m,
                    --HasOSKey r, MonadReader r m,
                    HasRsyncError e, HasIOException e, MonadError e m, Show e)
    => OSKey
    -> NamedSliceList
    -> [Slice]
    -> LocalRepository
    -> [BinPkgName]
    -> [BinPkgName]
    -> [String]
    -> m OSImage
debootstrap root distro extra repo include exclude components =
    do
      ePutStr (unlines [ "Creating clean build environment (" ++ codename (sliceListName distro) ++ ")"
                       , "  root: " ++ show (_root root)
                       , "  baseDist: " ++ show baseDist
                       , "  mirror: " ++ show mirror ])
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      (runV2 [$here] (shell cmd) B.empty >>= codefn) `catchError` (\(e :: e) -> error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show e))
      ePutStrLn "done."
      os <- createOSImage root distro extra repo -- arch?  copy?
      let sourcesPath' = view (to _root . rootPath) root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
          sources = prettyShow $ osFullDistro os
      liftIO $ replaceFile sourcesPath' sources
      _ <- liftIO $ useEnv (view (to _root . rootPath) root) return $ mapM addAptRepository extra
      return os
    where
      codefn (ExitSuccess, _, _) = return ()
      codefn e = error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show e)

      woot = view (to _root . rootPath) root
      wootNew = woot ++ ".new"
      baseDist = either id (codename . fst) . view sourceDist . head . map sliceSource . slices . sliceList $ distro
      mirror = uriToString' . view vendorURI . view sourceUri . head . map sliceSource . slices . sliceList $ distro
      cmd = intercalate " && "
              ["set -x",
               "rm -rf " ++ wootNew,
               ("debootstrap " ++
                (if include /= [] then "--include=" ++ intercalate "," (map unBinPkgName include) ++ " " else "") ++
                (if exclude /= [] then "--exclude=" ++ intercalate "," (map unBinPkgName exclude) ++ " " else "") ++
                "--variant=buildd " ++
                "--components=" ++ intercalate "," components ++ " " ++
                baseDist ++ " " ++
                wootNew ++ " " ++
                mirror),
               "cat " ++ wootNew ++ "/etc/apt/sources.list | sed -e 's/^deb /deb-src /' >>" ++ wootNew ++ "/etc/apt/sources.list",
               "mkdir -p " ++ woot,
               "rm -rf " ++ woot,
               "mv " ++ wootNew ++ " " ++ woot]
