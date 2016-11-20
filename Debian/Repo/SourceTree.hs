{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
module Debian.Repo.SourceTree
    ( addLogEntry
    , buildDebs
    , HasTopDir(topdir)
    , HasSubDir(subdir)
    , HasDebDir(debdir)
    , HasChangeLog(entry)
    , HasSourceTree(findSourceTree, copySourceTree)
    , HasBuildTree(findBuildTree)
    , explainSourcePackageStatus
    , findChanges
    , findDebianBuildTrees
    , findDebianSourceTrees
    , findOneDebianBuildTree
    , findOrigTarball
    , origTarballPath
    , BuildDecision(..)
    , SourcePackageStatus (Indep, All, None)
    , DebianBuildTree (debTree', topdir')
    , DebianSourceTree(tree', control')
    , SourceTree(dir')
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Exception (SomeException, try, throw)
import Control.Monad (foldM)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString as B
import Data.List (intercalate, nubBy, sortBy)
import Data.Time (NominalDiffTime)
import Debian.Changes (ChangeLogEntry(..), ChangesFile(..), parseEntries)
import Debian.Control.Policy (HasDebianControl(debianControl), DebianControl, parseDebianControlFromFile)
import Debian.Pretty (ppShow)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Changes (findChangesFiles)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.MonadOS (MonadOS(getOS))
import Debian.Repo.OSImage (osRoot)
import Debian.Repo.Prelude (getSubDirectories, replaceFile, dropPrefix)
import Debian.Repo.Prelude.Process (readProcessVE, timeTask, modifyProcessEnv)
import Debian.Repo.Prelude.Verbosity (noisier)
import Debian.Repo.Rsync (rsyncOld)
import qualified Debian.Version as V (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (hGetContents, IOMode(ReadMode), withFile)
import System.Process (CmdSpec(..), CreateProcess(cwd, cmdspec), proc, showCommandForUser)
import System.Unix.Chroot (useEnv)

class HasTopDir t where
    topdir :: t -> FilePath

class HasSubDir t where
    subdir :: t -> FilePath

class HasDebDir t where
    debdir :: t -> FilePath

class HasChangeLog t where
    entry :: t -> ChangeLogEntry

class HasSourceTree t where
    findSourceTree :: FilePath -> IO t
    -- ^ This just determines whether path is a directory and if so
    -- wraps SourceTree around it.
    copySourceTree :: t -> FilePath -> IO t

class HasBuildTree t where
    findBuildTree :: FilePath -> String -> IO t
    -- ^ This applies findSourceTree to path </> name.

-- |Find the .changes file which is generated by a successful run of
-- dpkg-buildpackage.
findChanges :: DebianBuildTree -> IO ChangesFile
findChanges tree =
    do let dir = topdir tree
       result <- findChangesFiles dir
       case result of
         [cf] -> return cf
         [] -> fail ("Couldn't find .changes file in " ++ dir)
         lst -> fail ("Multiple .changes files in " ++ dir ++ ": " ++ intercalate ", " (map ppShow lst))

-- |Rewrite the changelog with an added entry.
addLogEntry :: (HasChangeLog t, HasDebDir t) => ChangeLogEntry -> t -> IO ()
addLogEntry entry'' debtree =
-- readFile changelogPath >>= replaceFile changelogPath . ((show (pretty entry'')) ++)
  withFile changelogPath ReadMode (\ handle -> hGetContents handle >>= replaceFile changelogPath . ((ppShow entry'' ++ "\n\n") ++))
    where
      changelogPath = (debdir debtree) ++ "/debian/changelog"

-- |There are three possible results of a build: an upload consisting
-- of only the architecture independent debs (Indep), one including
-- both indep and binary debs (All), or with a failed build (None).
data SourcePackageStatus = All | Indep [BinPkgName] | None deriving (Show, Eq)

explainSourcePackageStatus :: SourcePackageStatus -> String
explainSourcePackageStatus All = "All architecture dependent files for the current build architecture are present."
explainSourcePackageStatus (Indep missing) = "Some or all architecture-dependent files for the current build architecture are missing: " ++ show missing
explainSourcePackageStatus None = "This version of the package is not present."

-- |Represents a decision whether to build a package, with a text juststification.
data BuildDecision
    = Yes String
    | No String
    | Arch String       -- Needs a -B build, architecture dependent files only
    | Auto String       -- Needs a 'automated' rebuild, with a generated version number and log entry
    | Error String      -- A fatal condition was encountered - e.g. a build dependency became older since last build

instance Show BuildDecision where
    show (Yes reason) = "Yes - " ++ reason
    show (No reason) = "No - " ++ reason
    show (Arch reason) = "Yes - " ++ reason
    show (Auto reason) = "Yes - " ++ reason
    show (Error reason) = "Error - " ++ reason

-- | Run dpkg-buildpackage in a build tree.
buildDebs :: (MonadOS m, MonadIO m) => Bool -> [(String, Maybe String)] -> DebianBuildTree -> BuildDecision -> m NominalDiffTime
buildDebs noClean setEnv buildTree decision =
    do
      root <- rootPath . osRoot <$> getOS
      noSecretKey <- liftIO $ getEnv "HOME" >>= return . (++ "/.gnupg") >>= doesDirectoryExist >>= return . not
      -- env0 <- liftIO getEnvironment
      -- Set LOGNAME so dpkg-buildpackage doesn't die when it fails to
      -- get the original user's login information
      let run cmd =
              liftIO $ do
                cmd' <- modifyProcessEnv (("LOGNAME", Just "root") : setEnv) cmd
                let cmd'' = cmd' {cwd = dropPrefix root path}
                timeTask $ useEnv root return $ readProcessVE cmd'' ("" :: String)
      _ <- liftIO $ run (proc "chmod" ["ugo+x", "debian/rules"])
      let buildCmd = proc "dpkg-buildpackage" (concat [["-sa"],
                                                       case decision of Arch _ -> ["-B"]; _ -> [],
                                                       if noSecretKey then ["-us", "-uc"] else [],
                                                       if noClean then ["-nc"] else []])
      (result, elapsed) <- liftIO . noisier 4 $ run buildCmd
      case result of
        (Right (ExitSuccess, _, _)) -> return elapsed
        result' -> fail $ "*** FAILURE: " ++ showCmd (cmdspec buildCmd) ++ " -> " ++ show result'
    where
      path = debdir buildTree
      showCmd (RawCommand cmd args) = showCommandForUser cmd args
      showCmd (ShellCommand cmd) = cmd

#if 0
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
#endif

findOrigTarball :: DebianBuildTree -> IO (Maybe FilePath)
findOrigTarball tree =
    do exists <- doesFileExist (origTarballPath tree)
       return $ if exists then Just (origTarballPath tree) else Nothing

origTarballPath :: DebianBuildTree -> FilePath
origTarballPath tree =
    topdir tree ++ "/" ++ orig
    where
      orig = name ++ "_" ++ version ++ ".orig.tar.gz"
      name = logPackage . entry $ tree
      version = V.version . logVersion . entry $ tree

-- deprecated
{-
copyDebianSourceTree :: (DebianSourceTreeC t) => t -> FilePath -> IO t
copyDebianSourceTree = copySourceTree

copyDebianBuildTree :: (DebianBuildTreeC t) => t -> FilePath -> IO t
copyDebianBuildTree = copySourceTree
-}

-- |Find all the debian source trees in a directory.
findDebianSourceTrees :: FilePath -> IO [(FilePath, DebianSourceTree)]
findDebianSourceTrees path =
    getSubDirectories path >>= \ (subdirs :: [FilePath]) ->
    foldM (\ pairs subdir'' ->
               try (findSourceTree (path </> subdir'')) >>=
               either (\ (_ :: SomeException) -> return pairs) (\ tree -> return ((subdir'', tree) : pairs))) [] subdirs

-- |Find all the debian source trees in a directory.
findDebianBuildTrees :: FilePath -> IO [DebianBuildTree]
findDebianBuildTrees path =
    getSubDirectories path >>=
    foldM (\ trees subdir'' ->
               try (findBuildTree path subdir'') >>=
               either (\ (_ :: SomeException) -> return trees) (\ tree -> return $ tree : trees)) []

-- |Find a DebianBuildTree inside a directory.  It finds all the
-- DebianSourceTrees, and if they all have the same package name it
-- returns the newest one according to the version numbers.  If there
-- are none, or there are trees with different package names, Nothing
-- is returned.
findOneDebianBuildTree :: FilePath -> IO (Maybe DebianBuildTree)
findOneDebianBuildTree path =
    do trees <- findDebianBuildTrees path
       -- Do all the trees have the same package name?
       case nubBy eqNames trees of
         -- Yes, return the newest one
         [_] -> return . Just . head . sortBy cmpVers $ trees
         -- No trees found
         [] -> return Nothing
         -- No, throw an exception
         names -> error $ "findOneDebianBuildTree: more than one source package name found in " ++ path ++ ": " ++ show (map (logPackage . entry) names)
    where
      eqNames tree1 tree2 = (logPackage . entry $ tree1) == (logPackage . entry $ tree2)
      cmpVers tree1 tree2 = compare (logVersion . entry $ tree1) (logVersion . entry $ tree2)

-- |Any directory containing source code.
data SourceTree =
    SourceTree {dir' :: FilePath} deriving Show

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
data DebianSourceTree =
    DebianSourceTree {tree' :: SourceTree,
                      control' :: DebianControl,
                      entry' :: ChangeLogEntry}
    deriving Show

instance HasDebianControl DebianSourceTree where
    debianControl = control'

-- |A Debian source tree plus a parent directory, which is where the
-- binary and source deb packages appear after a build.  Note that
-- topdir' </> subdir' == dir' . tree' . debTree'
data DebianBuildTree =
    DebianBuildTree {topdir' :: FilePath,
                     subdir' :: String,
                     debTree' :: DebianSourceTree}
    deriving Show

instance HasTopDir DebianSourceTree where
    topdir = dir' . tree'

instance HasDebDir DebianSourceTree where
    debdir = dir' . tree'

instance HasChangeLog DebianSourceTree where
    entry = entry'

instance HasDebDir DebianBuildTree where
    debdir t = topdir' t </> subdir' t

instance HasTopDir DebianBuildTree where
    topdir = topdir'

instance HasSubDir DebianBuildTree where
    subdir = subdir'

instance HasTopDir SourceTree where
    topdir = dir'

instance HasSourceTree SourceTree where
    findSourceTree path =
        doesDirectoryExist path >>= \ exists ->
        case exists of
          False -> fail $ "No such directory: " ++ path
          True -> return $ SourceTree path
    copySourceTree tree dest =
        createDirectoryIfMissing True dest >>
        rsyncOld [] (topdir tree) dest >>
        return (SourceTree dest)

instance HasSourceTree DebianSourceTree where
    findSourceTree path0 =
      findSourceTree path0 >>= \ (tree :: SourceTree) ->
      parseDebianControlFromFile (path0 ++ "/debian/control") >>= either throw return >>= \ c ->
      -- We only read part of the changelog, so be careful that the file
      -- descriptor gets closed.
      withFile (path0 ++ "/debian/changelog") ReadMode $ \ handle ->
          hGetContents handle >>= \ l ->
          case parseEntries l of
            (Right e : _) ->
              -- ePutStrLn ("findDebianSourceTree " ++ show path0 ++ " -> " ++ topdir tree) >>
              return (DebianSourceTree tree c e)
            (Left msgs : _) -> error $ "Bad changelog entry in " ++ show (path0 ++ "/debian/changelog") ++ ": " ++ intercalate ", " msgs
            [] -> return $ error $ "Empty changelog file: " ++ show (path0 ++ "/debian/changelog")
    copySourceTree tree dest =
        DebianSourceTree <$> copySourceTree (tree' tree) dest
                         <*> pure (control' tree)
                         <*> pure (entry' tree)

instance HasSourceTree DebianBuildTree where
    findSourceTree path =
        do trees <- findDebianBuildTrees path
           case nubBy eqNames trees of
             [_] -> return . head . sortBy cmpVers $ trees
             [] -> error $ "No source trees found in subdirectorys of " ++ path
             names -> error $ "Mutiple source package names found in " ++ path ++ ": " ++ show (map (logPackage . entry) names)
        where
          eqNames tree1 tree2 = (logPackage . entry $ tree1) == (logPackage . entry $ tree2)
          cmpVers tree1 tree2 = compare (logVersion . entry $ tree1) (logVersion . entry $ tree2)
    copySourceTree build dest =
        copySource >>= copyTarball >>= return . moveBuild
        where
          copySource = createDirectoryIfMissing True dest >> rsyncOld [] (topdir' build) dest
          -- copySource = DebianBuildTree <$> pure dest <*> pure (subdir' tree) <*> copySourceTree (debTree' tree) (dest </> subdir' tree)
          copyTarball (Right ExitSuccess, _, _) =
              do exists <- liftIO $ doesFileExist origPath
                 case exists of
                   False -> return (Right (ExitSuccess, B.empty, B.empty))
                   True -> liftIO $ readProcessVE (proc "cp" ["-p", origPath, dest ++ "/"]) B.empty
          copyTarball _result = error $ "Failed to copy source tree: " ++ topdir' build ++ " -> " ++ dest
          moveBuild (Right (ExitSuccess, _, _)) = build {topdir' = dest, debTree' = moveSource (debTree' build)}
          moveBuild _result = error $ "Failed to copy Tarball: " ++ origPath ++ " -> " ++ dest ++ "/"
          moveSource source = source {tree' = SourceTree {dir' = dest </> subdir build}}
          origPath = topdir build </> orig
          orig = name ++ "_" ++ version ++ ".orig.tar.gz"
          name = logPackage . entry $ build
          version = V.version . logVersion . entry $ build

instance HasBuildTree DebianBuildTree where
    findBuildTree path d = findSourceTree (path </> d) >>= return . DebianBuildTree path d

instance HasChangeLog DebianBuildTree where
    entry = entry' . debTree'
