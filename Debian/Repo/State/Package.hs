-- | Install packages to and delete packages from a local repository.
{-# LANGUAGE BangPatterns, ConstraintKinds, CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Debian.Repo.State.Package
    ( MonadInstall, getInstall, putInstall
    , evalInstall
    , scanIncoming
    , installPackages
    , deleteTrumped
    , deleteBinaryOrphans
    , deleteGarbage
    , deleteSourcePackages
    -- * Install result
    , InstallResult(..)
    , resultToProblems
    , explainError
    , explainErrors
    , showErrors
    , releaseKey
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception as E (try)
import Control.Lens ((%=), (.=), _1, _2, Lens', makeLenses, over, _Right, use, view)
import Control.Monad (filterM, foldM, when)
import Control.Monad.Except (catchError)
import Control.Monad.State (StateT, runStateT, MonadState(get, put))
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, fromChunks, readFile)
--import Data.Digest.Pure.MD5 (md5)
import Data.Digest.Pure.SHA ({-sha1,-} sha256)
import Data.Either (partitionEithers, lefts, rights)
import Data.List as List (filter, groupBy, intercalate, intersperse, isPrefixOf, isSuffixOf, map, partition, sortBy)
import Data.Map as Map (fromList, lookup)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Data.Set as Set (difference, empty, fold, fromList, insert, map, member, null, Set, size, toAscList, toList, union, unions, singleton)
import Data.Text as T (pack, Text, unpack)
import qualified Data.Text as T (concat)
import Data.Text.Encoding (encodeUtf8)
import Debian.Apt.Index (Compression(..), controlFromIndex)
import Debian.Arch (Arch(..), prettyArch)
import Debian.Changes (ChangedFileSpec(..), ChangesFile(..), changesFileName)
import Debian.Codename (Codename, codename)
import Debian.Control (ControlFunctions(stripWS), formatControl, formatParagraph, Paragraph')
import qualified Debian.Control.Text as B (appendFields, Control, Control'(Control), ControlFunctions(lookupP{-, parseControlFromHandle-}), parseControl, Field, Field'(Field), fieldValue, modifyField, Paragraph, raiseFields, renameField)
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile))
import Debian.Pretty (PP(..), ppPrint, ppShow)
import Extra.Except -- (HasIOException, MonadError)
import Debian.Relation (BinPkgName, PkgName)
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (parseSection', Section(..), sectionName, sectionName', SubSection(section))
import Debian.Repo.Changes (changeName, changePath, findChangesFiles)
import Debian.Repo.LocalRepository (Layout(..), LocalRepository, poolDir', repoLayout, repoReleaseInfoLocal, repoRoot)
import Debian.Repo.MonadRepos (HasReposState(..), MonadRepos, ReleaseKey, releaseByKey, putRelease)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID, PackageID(packageName, packageVersion), prettyPackageID)
import Debian.Repo.PackageIndex (binaryIndexes, BinaryPackage(packageID, packageInfo), BinaryPackage(BinaryPackage, pConflicts, pDepends, pPreDepends, pProvides, pReplaces), PackageIndex(..), packageIndexes, packageIndexPath, SourceControl(..), SourceFileSpec(SourceFileSpec, sourceFileName), sourceIndexes, SourcePackage(sourcePackageID), SourcePackage(SourcePackage, sourceControl, sourceDirectory, sourcePackageFiles, sourceParagraph))
import Debian.Repo.Prelude (nub')
import Debian.Repo.Prelude.Misc (listDiff)
import Debian.Repo.Prelude.GPGSign (PGPKey)
import Debian.Repo.Repo ({-Repo,-} repoArchList, repoKey, repoKeyURI)
import Debian.Repo.Release (Release(releaseAliases, releaseComponents, releaseName, releaseArchitectures))
import Debian.Repo.State.Release (findReleases, prepareRelease, writeRelease, signRepo)
import Debian.Repo.URI (fileFromURIStrict)
--import Debian.TH (here)
import Debian.URI (fromParseError, HasParseError, uriPathLens)
import Debian.VendorURI (vendorURI)
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Debian.Version.Text ()
import Extra.EnvPath (EnvPath, outsidePath)
import Extra.Verbosity (ePutStrLn, qPutStr, qPutStrLn)
import Network.URI (URI)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile, renameFile)
--import System.Exit (ExitCode(..))
import System.FilePath ((</>), splitFileName)
import System.FilePath.Extra2 (writeAndZipFileWithBackup)
import System.IO ()
import qualified System.Posix.Files as F (createLink, fileSize, getFileStatus)
import System.Posix.Types (FileOffset)
import System.Process (proc, shell, readCreateProcess)
import Text.Parsec (ParseError)
import "regex-compat-tdfa" Text.Regex (matchRegex, mkRegex, splitRegex)
import Text.PrettyPrint (Doc, text, hcat, punctuate)
import Distribution.Pretty (Pretty(pretty))

instance Pretty FileOffset where
    pretty = text . show

data InstallState
    = InstallState
      { _repository :: LocalRepository
      , _live :: Maybe (Set Text)
      -- ^ All the files that are part of this repository - debs,
      -- original tarballs, index files, etc.
      , _releases :: [Release]
      -- ^ All the releases in this repository.
      , _modified :: Set ReleaseKey
      -- ^ Releases which have been modified and need to be rewritten and signed
      }

$(makeLenses ''InstallState)

-- | A monad for installing or deleting a repository's packages
class HasInstallState s where installState :: Lens' s InstallState
instance HasInstallState InstallState where installState = id
type MonadInstall s m = (HasInstallState s, MonadState s m)

instance HasInstallState (InstallState, s) where installState = _1
instance HasReposState s => HasReposState (InstallState, s) where reposState = _2 . reposState

getInstall :: MonadInstall s m => m InstallState
getInstall = use installState

putInstall :: MonadInstall s m => InstallState -> m ()
putInstall x = installState .= x

runInstall :: (MonadIO m, MonadRepos s m) => StateT (InstallState, s) m a -> LocalRepository -> Maybe PGPKey -> m (a, InstallState)
runInstall task repo keyname = do
  releases <- findReleases repo
  s <- get
  (r, (st, s')) <- runStateT task (InstallState repo Nothing releases empty, s)
  put s'
  mapM_ (\ key -> releaseByKey key >>= \ rel -> liftIO (writeRelease repo rel >>= signRepo keyname repo)) (Set.toList (view modified st))
  return (r, st)

-- Evaluate a specific instance of MonadInstall
evalInstall :: (MonadIO m, MonadRepos s m) => StateT (InstallState, s) m a -> LocalRepository -> Maybe PGPKey -> m a
evalInstall task repo keyname = fst <$> runInstall task repo keyname

modifyInstall :: MonadInstall s m => (InstallState -> InstallState) -> m ()
modifyInstall f = installState %= f

data InstallResult
    = Ok
    | Failed [Problem]          -- Package can not currently be installed
    | Rejected [Problem]        -- Package can not ever be installed
    deriving Eq

data Problem
    = NoSuchRelease Codename
    | NoSuchSection Codename [Section]
    | ShortFile FilePath FileOffset FileOffset
    -- ^ A file in the incoming directory is shorter than its entry
    -- in the .changes file indicates - maybe it is still uploading?
    | LongFile FilePath FileOffset FileOffset
    -- ^ A file in the incoming directory is longer than its entry
    -- in the .changes file indicates.  Maybe it is left over from a
    -- previous failed upload, or a concurrent upload from somewhere
    -- else.  (That should not be permitted to happen, the .changes
    -- file should be a lock for uploads of that package/version.)
    | MissingFromIncoming FilePath
    -- ^ A file listed in the .changes file is missing from incoming.
    | MissingFromPool FilePath
    -- ^ A file listed in an index file is not in the pool
    | BadChecksum FilePath String String
    -- ^ A file which is listed in the .changes file exists in incoming
    -- and is the right length but its checksum is wrong.
    | InvalidControlInfo FilePath String
    -- ^ Badly formatted control file
    | DuplicatePackage Duplicate
    -- ^ A binary deb in incoming has the same ID as a deb already in
    -- the index - maybe an old source package has a binary package
    -- name that conflicts with the package being uploaded.
    | OtherProblem String
    deriving (Eq)

data Duplicate
    = Duplicate {_release :: Release, _index :: PackageIndex, _newPackage :: BinaryPackage, _oldPackage :: BinaryPackage} deriving (Eq, Show)

instance Pretty (PP Problem) where
    pretty (PP (NoSuchRelease rel)) = text "NoSuchRelease  " <> text (codename rel)
    pretty (PP (NoSuchSection rel sect)) = text "NoSuchSection " <> text (codename rel) <> text " " <> prettyList (fmap (text . sectionName') sect)
    pretty (PP (ShortFile path a b)) = text "ShortFile " <> text path <> text " " <> pretty a <> text " " <> pretty b
    pretty (PP (LongFile path a b)) = text "LongFile " <> text path <> text " " <> pretty a <> text " " <> pretty b
    pretty (PP (MissingFromIncoming path)) = text "MissingFromIncoming " <> text path
    pretty (PP (MissingFromPool path)) = text "MissingFromPool " <> text path
    pretty (PP (BadChecksum path a b)) = text "BadChecksum " <> text path <> text " " <> text a <> text " " <> text b
    pretty (PP (DuplicatePackage (Duplicate rel idx old _new))) = text "DuplicatePackage in " <> pretty rel <> text ":" <> pretty (PP idx) <> text ": " <> pretty (PP (packageID old)) <> " - does the old source deb (" <> pretty (PP (sourceIdent (rel, idx, old))) <> ") have a binary deb name that conflicts with the newer deb in incoming?  Try hiding the incoming directory and doing newdist --expire."
    pretty (PP (InvalidControlInfo path a)) = text "InvalidControlInfo " <> text path <> text " " <> text a
    pretty (PP (OtherProblem s)) = text "OtherProblem " <> text s

prettyList :: [Doc] -> Doc
prettyList xs = text "[" <> hcat (punctuate (text ", ") xs) <> text "]"

releaseKey :: (MonadRepos s m, MonadInstall s m) => Release -> m ReleaseKey
releaseKey release = do
  repo <- use (installState . repository)
  putRelease repo release

mergeResults :: [InstallResult] -> InstallResult
mergeResults results =
    doMerge Ok results
    where
      doMerge x [] = x
      doMerge x (Ok : more) = doMerge x more
      doMerge (Rejected p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Rejected p1) (Failed p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Failed p2 : more) = doMerge (Failed (p1 ++ p2)) more
      doMerge Ok (x : more) = doMerge x more

showErrors :: [InstallResult] -> String
showErrors = intercalate "\n" . List.map explainError . concatMap resultToProblems

-- | Return the list of issues that provoked a result - the Ok result
-- becomes the empty list, a Failed or Rejected result becomes a
-- non-empty list.
resultToProblems :: InstallResult -> [Problem]
resultToProblems Ok = []
resultToProblems (Failed x) = x
resultToProblems (Rejected x) = x

explainErrors :: [InstallResult] -> [String]
explainErrors = List.map explainError . concatMap resultToProblems

explainError :: Problem -> String
explainError (NoSuchRelease dist) =
    ("\nThe distribution in the .changes file (" ++ codename dist ++ ") does not exist.  There\n" ++
     "are three common reasons this might happen:\n" ++
     " (1) The value in the latest debian/changelog entry is wrong\n" ++
     " (2) A new release needs to be created in the repository.\n" ++
     "       newdist --root <root> --create-release " ++ codename dist ++ "\n" ++
     " (3) A new alias needs to be created in the repository (typically 'unstable', 'testing', or 'stable'.)\n" ++
     "       newdist --root <root> --create-alias <existing release> " ++ codename dist ++ "\n")
explainError (NoSuchSection dist components) =
    ("\nThe component" ++ plural "s" components ++ " " ++ intercalate ", " (List.map sectionName' components) ++
     " in release " ++ codename dist ++ " " ++
     plural "do" components ++ " not exist.\n" ++
     "either the 'Section' value in debian/control was wrong or the section needs to be created:" ++
     concat (List.map (\ component -> "\n  newdist --root <root> --create-section " ++ codename dist ++ "," ++ sectionName' component) components))
explainError (ShortFile path a b) =
    ("\nThe file " ++ path ++ "\n" ++
     "is shorter than it should be (expected: " ++ show a ++ ", actual: " ++ show b ++ ".)  This usually\n" ++
     "happens while the package is still being uploaded to the repository.")
explainError (LongFile path _ _) =
    ("\nThe file " ++ path ++
     "\nis longer than it should be.  This can happen when the --force-build\n" ++
     "option is used.  In this case the --flush-pool option should help.")
explainError (BadChecksum path _ _) =
    ("\nThe checksum of the file " ++ path ++ "\n" ++
     "is different from the value in the .changes file.\n" ++
     "This can happen when the --force-build option is used.  In this case the\n" ++
     "--flush-pool option should help.  It may also mean a hardware failure.")
explainError other = show (pretty (PP other))

plural :: String -> [a] -> String
plural "do" [_] = "does"
plural "do" _ = "do"

plural "s" [_] = ""
plural "s" _ = "s"

plural _ _ = ""

-- | Find all the .changes files in the incoming directory and try to
-- process each to install the package into a local repository.
scanIncoming :: (MonadIO m, MonadRepos s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Bool -> Maybe PGPKey -> LocalRepository -> m [(ChangesFile, InstallResult)]
scanIncoming createSections keyname repo = do
  qPutStrLn ("Uploading packages to " ++ outsidePath (view repoRoot repo) </> "incoming")
  changes <- liftIO (findChangesFiles (outsidePath (view repoRoot repo) </> "incoming"))
  case changes of
    [] -> qPutStrLn "Nothing to install."
    _ -> qPutStrLn ("To install:\n  " ++ (intercalate "\n  " . List.map ppShow $ changes))
  results <- installPackages createSections keyname repo changes
  case results of
    [] -> return ()
    _ -> qPutStrLn ("Upload results:\n  " ++ (intercalate "\n  " . List.map (uncurry showResult) $ (zip changes results)))
  return (zip changes results)
    where
      showResult changes result =
          changesFileName changes ++ ": " ++
          case result of
            Ok -> "Ok"
            Failed lst -> "Failed -\n      " ++ (intercalate "\n      " $ List.map (show . pretty . PP) lst)
            Rejected lst -> "Rejected -\n      " ++ (intercalate "\n      " $ List.map (show . pretty . PP) lst)

-- | Install several packages into a repository.  This means
-- 1. getting the list of files from the .changes file,
-- 2. verifying the file checksums,
-- 3. deleting any existing version and perhaps other versions which
--    were listed in the delete list,
-- 4. updating the Packages and Sources files, and
-- 5. moving the files from the incoming directory to the proper
--    place in the package pool.
installPackages :: forall s e m. (MonadIO m, MonadRepos s m, Show e, HasIOException e, HasParseError e, MonadError e m) =>
                   Bool                 -- ^ ok to create new releases and sections
                -> Maybe PGPKey         -- ^ key to sign repository with
                -> LocalRepository      -- ^ destination repository
                -> [ChangesFile]        -- ^ Packages to be installed
                -> m [InstallResult]    -- ^ Outcome of each source package
installPackages createSections keyname repo changeFileList =
    evalInstall
      (installPackages' createSections keyname repo changeFileList)
      repo
      keyname

installPackages' ::
    forall s e m. (MonadIO m, MonadRepos s m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m)
    => Bool                 -- ^ ok to create new releases and sections
    -> Maybe PGPKey         -- ^ key to sign repository with
    -> LocalRepository      -- ^ destination repository
    -> [ChangesFile]        -- ^ Packages to be installed
    -> m [InstallResult]    -- ^ Outcome of each source package
installPackages' createSections keyname repo changeFileList = do
  results' <- foldM (installFiles createSections) [] changeFileList
  results'' <- updateIndexes (reverse results')
  when (elem Ok results'')
       (do mapM_ (uncurry (finish (view repoRoot repo) (maybe Flat id (view repoLayout repo)))) (zip changeFileList results'')
           let releaseNames = nub' (List.map changeRelease changeFileList)
           releases' <- catMaybes <$> mapM findRelease' releaseNames
           mapM_ (\ rel -> liftIO $ writeRelease repo rel >>= signRepo keyname repo) releases')
  return results''
    where
      -- Update all the index files affected by the successful
      -- installs.  This is a time consuming operation, so we want to
      -- do this all at once, rather than one package at a time
      updateIndexes :: [InstallResult] -> m [InstallResult]
      updateIndexes results =
          do (pairLists :: [Either InstallResult [((Release, PackageIndex), B.Paragraph)]]) <-
                 mapM (uncurry buildInfo) (zip changeFileList results)
             let sortedByIndex = sortBy compareIndex (concat (rights pairLists))
             let groupedByIndex = undistribute (groupBy (\ a b -> compareIndex a b == EQ) sortedByIndex)
             result <- addPackagesToIndexes groupedByIndex
             case result of
               Ok -> return $ List.map (either id (const Ok)) pairLists
               problem -> return $ List.map (const problem) results
          where
            compareIndex :: ((Release, PackageIndex), B.Paragraph) -> ((Release, PackageIndex), B.Paragraph) -> Ordering
            compareIndex (a, _) (b, _) = compare a b

-- Build the control information to be added to the package indexes.
buildInfo :: (MonadIO m, MonadInstall s m) => ChangesFile -> InstallResult -> m (Either InstallResult [((Release, PackageIndex), B.Paragraph)])
buildInfo changes Ok =
          do mrel <- findRelease' (changeRelease changes)
             case mrel of
               Just release ->
                   do (info :: [Either InstallResult B.Paragraph]) <- mapM (fileInfo changes) indexFiles
                      case lefts info of
                        [] ->
                            let (pairs :: [(Set (Release, PackageIndex), Either InstallResult B.Paragraph)]) = zip (indexLists release) info in
                            let (pairs' :: [(Set (Release, PackageIndex), B.Paragraph)]) =
                                    catMaybes $ List.map (\ (a, b) -> either (const Nothing) (\ b' -> Just (a, b')) b) pairs in
                            let (pairs'' :: [((Release, PackageIndex), B.Paragraph)]) = concat (List.map distribute pairs') in
                            return (Right pairs'')
                        results -> return (Left (mergeResults results))
               Nothing -> return . Left . Failed $ [NoSuchRelease (changeRelease changes)]
          where
            indexLists :: Release -> [Set (Release, PackageIndex)]
            indexLists release = List.map (indexes release) indexFiles
            indexes :: Release -> ChangedFileSpec -> Set (Release, PackageIndex)
            indexes release file = Set.map (\ arch -> (release, PackageIndex (section . changedFileSection $ file) arch)) (archSet release changes file)
            indexFiles = dsc ++ debs
            (debs :: [ChangedFileSpec]) = filter f files
                where (f :: ChangedFileSpec -> Bool) = (isSuffixOf ".deb" . changedFileName)
                      (files :: [ChangedFileSpec]) = (changeFiles changes)
            dsc = filter (isSuffixOf ".dsc" . changedFileName) (changeFiles changes)
            -- (debs, nonDebs) = partition (isSuffixOf ".deb" . changedFileName) (changeFiles changes)
            -- (indepDebs, archDebs) = partition (isSuffixOf "_all.deb" . changedFileName) debs
            -- (dsc, other) = partition (isSuffixOf ".dsc" . changedFileName) nonDebs
            --fileIndex release file = List.map (PackageIndex release (section . changedFileSection $ file)) (archList release changes file)
buildInfo _ notOk = return . Left $ notOk

-- For a successful install this unlinks the files from INCOMING and
-- moves the .changes file into INSTALLED.  For a failure it moves
-- all the files to REJECT.
finish :: (MonadIO m) => EnvPath -> Layout -> ChangesFile -> InstallResult -> m ()
finish root layout changes Ok =
          do --vPutStrBl 1 stderr $ "  finish Ok " ++ changesFileName changes
             mapM_ (liftIO . removeFile . ((outsidePath root </> "incoming") </>) . changedFileName) (changeFiles changes)
             installChangesFile root layout changes
finish root _ changes (Rejected _) =
          do --vPutStrBl 1 stderr $ "  finish Rejected " ++ changesFileName changes
             mapM_ (\ name -> liftIO $ moveFile (outsidePath root </> "incoming" </> name) (outsidePath root </> "reject" </> name))
                      (List.map changedFileName (changeFiles changes))
             liftIO $ moveFile (outsidePath root </> "incoming" </> changeName changes) (outsidePath root </> "reject" </> changeName changes)
finish _ _ changes (Failed _) =
          do qPutStrLn $ "  Finish Failed " ++ changesFileName changes
             return ()

installChangesFile :: (MonadIO m) => EnvPath -> Layout -> ChangesFile -> m ()
installChangesFile root layout changes =
          liftIO (moveFile (changePath changes) dst)
          where dst = case layout of
                        Flat -> outsidePath root </> changeName changes
                        Pool -> outsidePath root </> "installed" </> changeName changes

incoming :: MonadInstall s m => m FilePath
incoming = do
  root <- view (repository . repoRoot) <$> getInstall
  return $ outsidePath root </> "incoming" {- </> changedFileName file -}

#if 0
reject :: MonadInstall s m => ChangedFileSpec -> m FilePath
reject file = do
  root <- view (repository . repoRoot) <$> getInstall
  return $ outsidePath root </> "reject" </> changedFileName file
#endif

-- | Get information about one of the (.deb or .dsc) files listed in a .changes file.
fileInfo :: (MonadIO m, MonadInstall s m) => ChangesFile -> ChangedFileSpec -> m (Either InstallResult B.Paragraph)
fileInfo changes file = do
  repo <- view repository <$> getInstall
  getControl >>= addFields repo
                where
                  getControl =
                      do -- path <- incoming file
                         dir <- incoming
                         control <-
                             case isSuffixOf ".deb" . changedFileName $ file of
                               -- Extract the control information from a .deb
                               True -> getDebControl file
                               -- This is the .dsc file, parse it
                               False -> liftIO $ S.parseControlFromFile (dir </> changedFileName file) >>= return . either (Left . show) Right
                         case control of
                           -- The control file should contain one paragraph
                           Right (S.Control [info]) -> return (Right info)
                           Left message -> return . Left . Rejected $ [InvalidControlInfo (dir </> changedFileName file) message]
                           Right (S.Control _) -> return . Left . Rejected $ [InvalidControlInfo (dir </> changedFileName file) "Expected one paragraph"]
                  addFields :: MonadInstall s m => LocalRepository -> (Either InstallResult B.Paragraph) -> m (Either InstallResult B.Paragraph)
                  addFields _ (Left result) = return $ Left result
                  addFields repo (Right info) =
                      case isSuffixOf ".deb" . changedFileName $ file of
                        True -> addDebFields repo changes file info
                        False -> addSourceFields repo changes file info
                  -- | Extract the control file from a binary .deb.

getDebControl :: forall s m. (MonadIO m, MonadInstall s m) => ChangedFileSpec -> m (Either String B.Control)
getDebControl file = do
  dir <- incoming
  let path = dir </> changedFileName file
  r <- liftIO (try (getDebControl' path)) :: m (Either IOError (Either ParseError B.Control))
  return $ (either (Left . show) (either (Left . show) Right)) r

-- let path = "/home/dsf/.autobuilder/localpools/bionic-seereason/incoming/haskell-devscripts-minimal_0.13.8-0+seereason1~bionic1_all.deb"
getDebControl' :: FilePath -> IO (Either ParseError B.Control)
getDebControl' path = do
  names <- (filter (isPrefixOf "control.tar.") . lines) <$> readCreateProcess (proc "ar" ["t", path]) ""
  let (name, c) =
          case names of
            ["control.tar.gz"] -> ("control.tar.gz", "z")
            ["control.tar.xz"] -> ("control.tar.xz", "J")
            _ -> error ("Unexpected result from ar t " ++ path)
  text <- readCreateProcess (shell ("ar p " ++ path ++ " " ++ name ++ " | tar x" ++ c ++ "O ./control")) ""
  return $ B.parseControl "" (pack text)

findRelease' :: MonadInstall s m => Codename -> m (Maybe Release)
findRelease' name = do
    rels <- view releases <$> getInstall
    return $ findRelease rels name

findRelease :: [Release] -> Codename -> Maybe Release
findRelease releases name =
    case filter (\ release -> elem name (releaseName release : releaseAliases release)) releases of
      [] -> Nothing
      (x : _) -> Just x
      _ -> error $ "Internal error 16 - multiple releases named " ++ codename name

#if 0
markReleaseModified :: MonadInstall s m => ReleaseKey -> m ()
markReleaseModified = modifyInstall . over modified . Set.insert
#endif

-- | Hard link the files of each package into the repository pool,
-- but don't unlink the files in incoming in case of subsequent
-- failure.
installFiles :: forall s e m. (MonadIO m, MonadRepos s m, MonadInstall s m, HasIOException e, MonadError e m) => Bool -> [InstallResult] -> ChangesFile -> m [InstallResult]
installFiles createSections results changes = do
  mrel <- findOrCreateRelease (changeRelease changes)
  maybe (return (Failed [NoSuchRelease (changeRelease changes)] : results)) (installFiles' createSections changes results) mrel
    where
      findOrCreateRelease :: Codename -> m (Maybe Release)
      findOrCreateRelease name = do
        rels <- view releases <$> getInstall
        case createSections of
            False -> return (findRelease rels name)
            True -> do let release = findRelease rels name
                       repo <- (view repository <$> getInstall)
                       case release of
                         Nothing ->
                             do newRelease <- prepareRelease repo name [] [parseSection' "main"] (repoArchList repo)
                                modifyInstall (over releases (newRelease :))
                                return (Just newRelease)
                         Just release -> return (Just release)

installFiles' :: (MonadIO m, MonadRepos s m, MonadInstall s m, HasIOException e, MonadError e m) => Bool -> ChangesFile -> [InstallResult] -> Release -> m [InstallResult]
installFiles' createSections changes results release =
                let sections = nub' . List.map (section . changedFileSection) . changeFiles $ changes in
                case (createSections, listDiff sections (releaseComponents release)) of
                  (_, []) -> installFiles'' changes results
                  (True, missing) ->
                      do qPutStrLn ("Creating missing sections: " ++ intercalate " " (List.map sectionName' missing))
                         repo <- view repository <$> getInstall
                         _release <- prepareRelease repo (releaseName release) [] missing (releaseArchitectures release)
                         installFiles'' changes results
                  (False, missing) ->
                      return (Failed [NoSuchSection (releaseName release) missing] : results)

installFiles'' :: (MonadIO m, MonadInstall s m) => ChangesFile -> [InstallResult] -> m [InstallResult]
installFiles'' changes results = do
  repo <- view repository <$> getInstall
  result <- mapM (installFile changes) (changeFiles changes) >>= return . mergeResults
  when (result == Ok) (modifyInstall (over live (Just . Set.union (paths repo) . fromMaybe empty)))
  return $ result : results
    where
      paths repo = Set.fromList $ List.map (T.pack . ((outsidePath (view repoRoot repo)) </>) . poolDir' repo changes) (changeFiles changes)

-- | Move one file into the repository
installFile :: (MonadIO m, MonadInstall s m) => ChangesFile -> ChangedFileSpec -> m InstallResult
installFile changes file = do
  repo <- view repository <$> getInstall
  live' <- view live <$> getInstall
  let root = view repoRoot repo
  let dir = outsidePath root </> poolDir' repo changes file
  let src = outsidePath root </> "incoming" </> changedFileName file
  let dst = dir </> changedFileName file
  installed <- liftIO $ doesFileExist dst
  available <- liftIO $ doesFileExist src
  let indexed = maybe False (Set.member (T.pack dst)) live' -- Is the file already part of the repository?
  case (available, indexed, installed) of
    (False, _, _) -> do                        -- Perhaps this file is about to be uploaded
      return (Failed [MissingFromIncoming src])
    (True, False, False) -> do         -- This just needs to be installed
      liftIO (createDirectoryIfMissing True dir)
      liftIO (F.createLink src dst)
      return Ok
    (True, False, True) -> do          -- A garbage file is already present
      qPutStrLn ("  Replacing unlisted file: " ++ dst)
      liftIO (removeFile dst)
      liftIO (F.createLink src dst)
      return Ok
    (True, True, False) -> do
      -- The repository is damaged - the file is listed in the index
      -- but it does not exist at that location.
      return (Failed [MissingFromPool dst])
    (True, True, True) -> do           -- Further inspection is required
      installedSize <- liftIO $ F.getFileStatus dst >>= return . F.fileSize
      --installedMD5sum <- liftIO $ L.readFile dst >>= return . show . md5
      --installedSHA1sum <- liftIO $ L.readFile dst >>= return . show . sha1
      installedSHA256sum <- liftIO $ L.readFile dst >>= return . show . sha256
      let status =
              case (compare (changedFileSize file) installedSize, compare (changedFileSHA256sum file) installedSHA256sum) of
                -- Somehow the correct file is already installed - so be it.
                (EQ, EQ) -> Ok
                -- The wrong file of the right length is installed
                (EQ, _) -> Rejected [BadChecksum dst (changedFileSHA256sum file) installedSHA256sum]
                -- File may be in the process of being uploaded
                (LT, _) -> Failed [ShortFile dst (changedFileSize file) installedSize]
                -- This must be the wrong file
                (GT, _) -> Rejected [LongFile dst (changedFileSize file) installedSize]
      return status

archSet :: Release -> ChangesFile -> ChangedFileSpec -> Set Arch
archSet release changes file =
    case () of
      _ | isSuffixOf "_all.deb" name -> releaseArchitectures release
      _ | isSuffixOf ".deb" name -> singleton (changeArch changes)
      _ | isSuffixOf ".udeb" name -> empty
      _ -> singleton Source
    where name = changedFileName file

distribute :: (Set a, b) -> [(a, b)]
distribute (iset, p) = List.map (\ i -> (i, p)) (toList iset)

undistribute :: [[(a, b)]] -> [(a, [b])]
-- undistribute pairss = Map.toList (Map.fromListWith (++) (map (\ (a, b) -> (a, [b])) (concat pairss)))
undistribute [] = []
undistribute ([] : tail) = undistribute tail
undistribute (((index, info) : items) : tail) =
    (index, info : List.map snd items) : undistribute tail

addDebFields :: MonadInstall s m => LocalRepository -> ChangesFile -> ChangedFileSpec -> Paragraph' Text -> m (Either InstallResult (Paragraph' Text))
addDebFields repo changes file info =
    let (binaryVersion :: DebianVersion) =
            maybe (error $ "Missing 'Version' field") parseDebianVersion' (B.fieldValue "Version" info) in
    let (newfields :: [B.Field]) =
            [B.Field (T.pack "Source", " " <> source <> T.pack (versionSuffix binaryVersion)),
             B.Field (T.pack "Filename", T.pack (" " ++ poolDir' repo changes file </> changedFileName file)),
             B.Field (T.pack "Size", T.pack (" " ++ show (changedFileSize file))),
             B.Field (T.pack "SHA256", T.pack (" " ++ changedFileSHA256sum file)),
             B.Field (T.pack "SHA1", T.pack (" " ++ changedFileSHA1sum file)),
             B.Field (T.pack "MD5sum", T.pack (" " ++ changedFileMD5sum file))] in
    return $ Right $ B.appendFields newfields info
    where
      versionSuffix :: DebianVersion -> String
      versionSuffix binaryVersion = if binaryVersion /= sourceVersion then " (" ++ show (prettyDebianVersion sourceVersion) ++ ")" else ""
      source = maybe (error "Missing 'Source' field in .changes file") id (B.fieldValue "Source" (changeInfo changes))
      sourceVersion = changeVersion changes


addSourceFields :: MonadInstall s m => LocalRepository -> ChangesFile -> ChangedFileSpec -> B.Paragraph -> m (Either InstallResult B.Paragraph)
addSourceFields repo changes file info =
    return . Right . append . raise . modify . rename $ info
    where
      rename = B.renameField (T.pack "Source") (T.pack "Package")
      modify = B.modifyField (T.pack "Files") (\ b -> (T.pack (T.unpack b ++ "\n " ++ filesLine file))) .
               B.modifyField (T.pack "Checksums-Sha1") (\ b -> (T.pack (T.unpack b ++ "\n " ++ sha1Line file))) .
               B.modifyField (T.pack "Checksums-Sha256") (\ b -> (T.pack (T.unpack b ++ "\n " ++ sha256Line file)))
      raise = B.raiseFields (== (T.pack "Package"))
      append = B.appendFields $
               [B.Field (T.pack "Priority", T.pack (" " ++ changedFilePriority file)),
                B.Field (T.pack "Section", T.pack  (" " ++ (sectionName (changedFileSection file)))),
                B.Field (T.pack "Directory", T.pack (" " ++ poolDir' repo changes file))] ++
               maybe [] (\ s -> [B.Field (T.pack "Build-Info", " " <> s)]) (B.fieldValue "Build-Info" (changeInfo changes))
      filesLine file = changedFileMD5sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha1Line file = changedFileSHA1sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha256Line file = changedFileSHA256sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
{-
    let info' = B.renameField (T.pack "Source") (T.pack "Package") info in
    let info'' = B.modifyField (T.pack "Files") (\ b -> (T.pack (B.unpack b ++ "\n " ++ changedFileMD5sum file ++ " "  ++
                                                                 show (changedFileSize file) ++ " " ++
                                                                 changedFileName file))) info' in
    let info''' = B.raiseFields (== (T.pack "Package")) info'' in
    let newfields = [B.Field (T.pack "Priority", T.pack (" " ++ changedFilePriority file)),
                     B.Field (T.pack "Section", T.pack  (" " ++ (sectionName (changedFileSection file)))),
                     B.Field (T.pack "Directory", T.pack (" " ++ poolDir' release changes file))] ++
                    maybe [] (\ s -> [B.Field (T.pack "Build-Info", T.pack (" " ++ s))])
                              (B.fieldValue "Build-Info" (changeInfo changes)) in
    Right $ B.appendFields newfields info'''
-}

moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
    do --vPutStrBl 1 stderr ("moveFile " ++ src ++ " " ++ dst)
       doesFileExist dst >>= (flip when) (removeFile dst)
       F.createLink src dst
       removeFile src

-- |Add control information to several package indexes, making sure
-- that that no duplicate package ids are inserted.
addPackagesToIndexes :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => [((Release, PackageIndex), [B.Paragraph])] -> m InstallResult
addPackagesToIndexes pairs =
    do repo <- view repository <$> getInstall
       oldPackageLists <- mapM (uncurry getPackages_) indexKeys
       case partitionEithers oldPackageLists of
         -- No errors
         ([], oldPackageLists') ->
             do -- if none of the new packages are already in the index, add them
                let newPackageLists = List.map (\ ((release, index), info) -> List.map (toBinaryPackage_ release index) info) pairs
                    dupes :: [Duplicate]
                    dupes = concat (List.map (\ ((rel, idx), old, new) -> findDupes rel idx old new) (zip3 indexKeys oldPackageLists' newPackageLists))
                case dupes of
                  [] -> do mapM_ (updateIndex repo) (zip3 indexKeys oldPackageLists' newPackageLists)
                           return Ok
                  dupes -> return $ Failed (List.map DuplicatePackage dupes)
         (bad, _) -> return $ Failed (List.map (OtherProblem . show) bad)
    where
      findDupes :: Release -> PackageIndex -> [BinaryPackage] -> [BinaryPackage] -> [Duplicate]
      findDupes rel idx oldList newList =
          -- Assuming here that each package in oldList has a unique packageID
          let oldMap = Map.fromList (zip (List.map packageID oldList) oldList) in
          mapMaybe (\ new -> fmap (Duplicate rel idx new) (Map.lookup (packageID new) oldMap)) newList
          -- (\ (oldList, newList) -> filter (\ new -> any (== (packageID new)) (List.map packageID oldList)) newList)
      updateIndex repo ((release, index), oldPackages, newPackages) = liftIO $ putPackages_ repo release index (oldPackages ++ newPackages)
      indexKeys = List.map fst pairs
#if 0
      indexMemberFn :: [BinaryPackage] -> BinaryPackage -> Bool
      indexMemberFn packages package = any (== (packageID package)) (List.map packageID packages)
#endif

-- Repository Accessors and Inquiries

-- | Return a list of all the files in a release which are
-- 'live', in the sense that they appear in some index files.
findLive :: (MonadIO m, MonadRepos s m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => m (Set Text)
findLive = do
    repo <- view repository <$> getInstall
    case view repoLayout repo of
      Nothing -> return Set.empty       -- Repository is empty
      Just layout ->
          do !releases <- findReleases repo
             !sourcePackages <- mapM releaseSourcePackages_ releases >>= return . Set.unions
             !binaryPackages <- mapM releaseBinaryPackages_ releases >>= return . Set.unions
             let sourceFiles = Set.map (T.pack (outsidePath (view repoRoot repo) ++ "/") <>) . Set.map T.pack . Set.fold Set.union Set.empty . Set.map sourceFilePaths_ $ sourcePackages
             let binaryFiles = Set.map (T.pack (outsidePath (view repoRoot repo) ++ "/") <>) . Set.fold (\ mt s -> maybe s (`Set.insert` s) mt) Set.empty $ Set.map (B.fieldValue "Filename" . packageInfo) binaryPackages
             let changesFiles = Set.map T.pack . Set.fold Set.union Set.empty $ Set.map (Set.fromList . changesFilePaths (view repoRoot repo) layout releases) sourcePackages
             let uploadFiles = Set.map T.pack . Set.fold Set.union Set.empty . Set.map (uploadFilePaths (view repoRoot repo) releases) $ sourcePackages
             return $ Set.unions [sourceFiles, binaryFiles, changesFiles, uploadFiles]
    where
      changesFilePaths root Flat releases package =
          List.map ((outsidePath root ++ "/") ++) . changesFileNames releases $ package
      changesFilePaths root Pool releases package =
          List.map ((outsidePath root ++ "/installed/") ++) . changesFileNames releases $ package
      changesFileNames releases package =
          List.map (\ arch -> intercalate "_" [ppShow (packageName . sourcePackageID $ package),
                                               show (prettyDebianVersion . packageVersion . sourcePackageID $ package),
                                               show (prettyArch arch)] ++ ".changes") (toList (architectures releases))
      uploadFilePaths root releases package = Set.map ((outsidePath root ++ "/") ++) . uploadFileNames releases $ package
      uploadFileNames releases package =
          Set.map (\ arch -> intercalate "_" [ppShow (packageName . sourcePackageID $ package),
                                              show (prettyDebianVersion . packageVersion . sourcePackageID $ package),
                                              show (prettyArch arch)] ++ ".upload") (architectures releases)
      architectures releases = unions . List.map releaseArchitectures $ releases

instance (Pretty (PP r){-, Repo r-}) => Pretty (PP (r, Release, PackageIndex)) where
    pretty (PP (repo, r, i)) = text $
        intercalate "/" [ppShow repo,
                         "dist",
                         (codename . releaseName $ r),
                         ppShow (packageIndexComponent i),
                         show (prettyArch (packageIndexArch i))]

instance Pretty (PP (Release, PackageIndex)) where
    pretty (PP (r, i)) = text $
        intercalate "/" [(codename . releaseName $ r),
                         ppShow (packageIndexComponent i),
                         show (prettyArch (packageIndexArch i))]

instance (Pretty (PP r){-, Repo r-}) => Pretty (PP (r, Release)) where
    pretty (PP (repo, r)) = hcat [ppPrint repo, text " ", ppPrint r]

instance Pretty (PP Release) where
    pretty (PP r) = text $ intercalate " " (codename (releaseName r) : List.map ppShow (releaseComponents r))

instance Pretty (PP (Release, PackageIndex, PackageID BinPkgName)) where
    pretty (PP (r, i, b)) = text $
        intercalate "/" [(codename . releaseName $ r),
                         ppShow (packageIndexComponent i),
                         show (prettyArch (packageIndexArch i)),
                         ppShow b]

instance PkgName name => Pretty (PP (PackageID name)) where
    pretty (PP p) = prettyPackageID p -- packageName p ++ "=" ++ show (prettyDebianVersion (packageVersion p))

instance Pretty (PP BinaryPackage) where
    pretty (PP p) = ppPrint (packageID p)

-- |Delete any packages from a dist which are trumped by newer
-- packages.  These packages are not technically garbage because they
-- can still be installed by explicitly giving their version number to
-- apt, but it is not really a good idea to use them.
deleteTrumped :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Bool -> Maybe PGPKey -> [Release] -> m [Release]
deleteTrumped _ _ [] = error "deleteTrumped called with empty release list"
deleteTrumped dry keyname releases =
    mapM findTrumped releases >>=
    return . partitionEithers >>=
    \ (bad, good) ->
        case bad of
          [] -> return (concat good) >>=
                ifEmpty (qPutStr "deleteTrumped: nothing to delete") >>=
                deleteSourcePackages dry keyname . (List.map (\ (r, i, p) -> (r, i, packageID p)))
          _ -> error $ "Error reading package lists"
    where
      ifEmpty :: MonadIO m => m () -> [a] -> m [a]
      ifEmpty action [] = do action; return []
      ifEmpty _ x = return x

-- | Return a list of packages in a release which are trumped by some
-- newer version.
findTrumped :: forall s e m. (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Release -> m (Either String [(Release, PackageIndex, BinaryPackage)])
findTrumped release =
    do
      mapM doIndex (sourceIndexes release) >>= return . merge
    where
      doIndex :: PackageIndex -> m (Either e [(Release, PackageIndex, BinaryPackage)])
      doIndex index = getPackages_ release index >>= return . either Left (Right . (List.map (\ b -> (release, index, b))))

-- |Delete any packages from a dist which are trumped by newer
-- packages.  These packages are not technically garbage because they
-- can still be installed by explicitly giving their version number to
-- apt, but it is not really a good idea to use them.
deleteBinaryOrphans :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Bool -> Maybe PGPKey -> [Release] -> m ()
deleteBinaryOrphans _ _ [] = error "deleteBinaryOrphans called with empty release list"
deleteBinaryOrphans dry keyname releases =
    do -- All the source packages in the repository
       ((exns1, sourcePackages) :: ([[e]], [[[(Release, PackageIndex, SourcePackage)]]])) <- unzip <$> mapM (\ release -> partitionEithers <$> mapM (sourcePackagesOfIndex' release) (sourceIndexes release)) releases
       -- All the binary packages in the repository
       ((exns2, binaryPackages) :: ([[e]], [[[(Release, PackageIndex, BinaryPackage)]]])) <- unzip <$> mapM (\ release -> partitionEithers <$> mapM (getPackages' release) (binaryIndexes release)) releases
       case (concat exns1, concat exns2, concat (concat sourcePackages), concat (concat binaryPackages)) of
         ([], [], sps, bps) ->
             do let bps' = Set.fromList (List.map (\ (r, i, b) -> (r, i, packageID b)) bps)
                qPutStrLn ("Number of source packages: " ++ show (length sps))
                qPutStrLn ("Number of binary packages: " ++ show (size bps'))
                let -- The binary packages which are associated with
                    -- some source package.  These need to have their
                    -- architecture set from the release architecture
                    -- list and the source package architecture
                    goodBps :: Set (Release, PackageIndex, PackageID BinPkgName)
                    goodBps =
                        Set.fromList (concatMap f sps)
                        where
                          f (r, i, p) = concatMap (g r i) (sourcePackageBinaryIDs_ p)
                          g r i p' = List.map (h r i p') (toList (releaseArchitectures r))
                          h r i p' a = (r, i {packageIndexArch = a}, p')
{-
                        Set.fromList (concatMap (\ (r, i, p) -> Set.fromList (concatMap (\ bid -> map (\ a -> (r, i {packageindexArch = a}, bid)) (releaseArchitectures r)) (sourcePackageBinaryIDs p)) in

                        Set.fromList (concatMap (\ (r, i, p) -> List.map (\ (a, p') -> (r, (i {packageIndexArch = a}), p')) (concatMap (\ a -> (a, sourcePackageBinaryIDs p)) (releaseArchitectures r))) sps)
-}
                    badBps :: Set (Release, PackageIndex, PackageID BinPkgName)
                    badBps = Set.difference bps' goodBps
                qPutStrLn ("deleteBinaryOrphans - keeping " ++ show (Set.size goodBps) ++ " packages.")
                qPutStrLn ("deleteBinaryOrphans - discarding " ++ show (Set.size badBps) ++ " packages.")
                deleteBinaryPackages dry keyname badBps
         (exns1', exns2', _, _) -> error $ "Failure(s) loading package indexes:\n " ++ intercalate "\n " (List.map show (exns1' ++ exns2'))
    where
      -- p :: (Release, PackageIndex, PackageID BinPkgName) -> Bool
      -- p (_, _, pid) = isInfixOf "fay" (unBinPkgName . packageName $ pid)
      getPackages' release index = either Left (Right . List.map (\ p -> (release, index, p))) <$> getPackages_ release index
      sourcePackagesOfIndex' release index =
          (over _Right (List.map (\ p -> (release, index, p))) <$> sourcePackagesOfIndex_ release index) `catchError` (return . Left)
{-
         (Right sourcePackages', Right binaryPackages') ->
             do 
    mapM (findBinaryOrphans repo) releases >>=
    return . partitionEithers >>=
    \ (bad, good) ->
        case bad of
          [] -> return (concat good) >>=
                ifEmpty (qPutStr "deleteBinaryOrphans: nothing to delete") >>=
                deleteBinaryPackages dry keyname repo . (List.map (\ (r, i, p) -> (r, i, packageID p)))
          _ -> error $ "Error reading package lists"
    where
      ifEmpty :: IO () -> [a] -> IO [a]
      ifEmpty action [] = do action; return []
      ifEmpty _ x = return x
-}

-- | Return a list of binary packages in a release which have no
-- corresponding source package.  This never ought to happen, but due
-- to a broken newdist it did.
{-
findBinaryOrphans :: LocalRepository -> Release -> IO (Either String [(Release, PackageIndex, BinaryPackage)])
findBinaryOrphans repo release =
    mapM (doIndex (sourceIndexList release)) (binaryIndexList release) >>= return . merge
    where
      doIndex sourceIndexes binaryIndex =
          getPackages (repoKey repo) release binaryIndex >>=
          return . either Left (Right . (List.map (\ b -> (release, binaryIndex, b))))
-}

merge :: Show e => [Either e [(Release, PackageIndex, BinaryPackage)]] -> Either String [(Release, PackageIndex, BinaryPackage)]
merge packages =
    case partitionEithers packages of
      ([], packages') -> Right . concat . List.map tail . List.map newestFirst . groupByName . concat $ packages'
      (bad, _) -> Left $ "Error(s) reading source indexes: " ++ intercalate ", " (List.map show bad)
    where
      groupByName :: [(Release, PackageIndex, BinaryPackage)] -> [[(Release, PackageIndex, BinaryPackage)]]
      groupByName = groupBy equalNames . sortBy compareNames
      equalNames (_, _, a') (_, _, b') = packageName (packageID a') == packageName (packageID b')
      compareNames (_, _, a') (_, _, b') = compare (packageName (packageID a')) (packageName (packageID b'))
      newestFirst = sortBy (flip compareVersions)
      compareVersions (_, _, a') (_, _, b') = compare (packageVersion (packageID a')) (packageVersion (packageID b'))

-- | Collect files that no longer appear in any package index and move
-- them to the removed directory.  The .changes files are treated
-- specially: they don't appear in any index files, but the package
-- they belong to can be constructed from their name.
deleteGarbage :: (MonadIO m, MonadRepos s m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => m ()
deleteGarbage = do
  layout <- view (repository . repoLayout) <$> getInstall
  root <- view (repository . repoRoot) <$> getInstall
  case layout of
      Just layout ->
          do
            qPutStrLn ("deleteGarbage in " ++ outsidePath root ++ " (layout=" ++ show layout ++ ")")
            allFiles1 <- liftIO $ poolFiles root layout
            allFiles2 <- liftIO $ changesFileList root layout
            let allFiles = allFiles1 ++ allFiles2
            -- ePutStr ("allFiles:\n  " ++ intercalate "\n  " (sort allFiles) ++ "\n")
            liveFiles <- findLive
            -- ePutStr ("liveFiles:\n  " ++ intercalate "\n  " (sort liveFiles) ++ "\n")
            let deadFiles = Set.difference (Set.map T.pack (Set.fromList allFiles)) liveFiles
            qPutStrLn ("Removing:\n  " ++ intercalate "\n  " (Set.toAscList (Set.map T.unpack deadFiles)) ++ "\n")
            mapM_ (liftIO . moveToRemoved root . T.unpack) (Set.toList deadFiles)
      _ -> error "Cannot remove files from an empty repository"
    where
      poolFiles root Flat = getDirectoryContents (outsidePath root) >>=
                            filterM (doesFileExist . ((outsidePath root) </>))
      poolFiles root Pool =
          getSubPaths (outsidePath root </> "pool") >>=
          mapM getSubPaths >>= return . concat >>=
          mapM getSubPaths >>= return . concat >>=
          mapM getSubPaths >>= return . concat
      changesFileList root Pool = getDirectoryPaths (outsidePath root ++ "/installed")
      -- In this case we already got the .changes files from the top directory
      changesFileList root Flat = getDirectoryPaths (outsidePath root) >>= return . List.filter (isSuffixOf ".changes")
      getSubPaths path =
          do
            isDir <- doesDirectoryExist path
            case isDir of
              False -> return [path]
              True -> getDirectoryPaths path
      getDirectoryPaths dir = getDirectoryContents dir >>= return . List.filter filterDots >>= return . List.map (dir </>)
      filterDots "." = False
      filterDots ".." = False
      filterDots _ = True
      -- upload files only appear when we dupload from a flat repository to another.
      moveToRemoved root file =
          renameFile file (outsidePath root </> "removed" </> snd (splitFileName file))

-- | Delete specific source packages and their associated binary packages.
deleteSourcePackages :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Bool -> Maybe PGPKey -> [(Release, PackageIndex, PackageID BinPkgName)] -> m [Release]
deleteSourcePackages _ _ [] = return []
deleteSourcePackages dry keyname packages =
    do qPutStrLn ("deleteSourcePackages:\n " ++ intercalate "\n " (List.map (ppShow . (\ (_, _, x) -> x)) packages))
       releases <- (Set.fromList . view (repository . repoReleaseInfoLocal)) <$> getInstall
       mapM doIndex (Set.toList (allIndexes releases))
    where
      doIndex (release, index) = getEntries release index >>= put release index . List.partition (victim release index)
      put :: (MonadIO m, MonadInstall s m) => Release -> PackageIndex -> ([BinaryPackage], [BinaryPackage]) -> m Release
      put release index ([], _) = do
        qPutStrLn ("deleteSourcePackages - nothing to remove from " ++ show index)
        return release
      put release index (junk, keep) = do
        qPutStrLn ("deleteSourcePackages  - Removing packages from " ++ ppShow (release, index) ++ ":\n  " ++ intercalate "\n " (List.map (ppShow . packageID) junk))
        putIndex' keyname release index keep
      allIndexes releases = Set.fold Set.union Set.empty (Set.map (\ r -> Set.fromList (List.map (r,) (packageIndexes r))) releases) -- concatMap allIndexes (Set.toList indexes)
      -- (indexes, invalid) = Set.partition (\ (_, i) -> packageIndexArch i == Source) (Set.fromList (List.map (\ (r, i, _) -> (r, i)) (repoReleaseInfoLocal repo)))
      -- (source, invalid) = Set.partition (\ (r, i, b) -> packageIndexArch i == Source) (Set.fromList packages)
      -- (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (List.map fst packages))
      -- allIndexes (release, sourceIndex) = packageIndexList release

      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: Release -> PackageIndex -> BinaryPackage -> Bool
      victim release index binaryPackage = Set.member (sourceIdent (release, index, binaryPackage)) (Set.fromList packages)
      getEntries :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Release -> PackageIndex -> m [BinaryPackage]
      getEntries release index = getPackages_ release index >>= return . either (error . show) id
      putIndex' :: (MonadIO m, MonadInstall s m) => Maybe PGPKey -> Release -> PackageIndex -> [BinaryPackage] -> m Release
      putIndex' keyname release index entries = do
        -- markReleaseModified
        repo <- view repository <$> getInstall
        case dry of
          True -> ePutStrLn ("dry run: not changing " ++ show index)
          False -> liftIO $ putIndex (view repoRoot repo) release index entries >> writeRelease repo release >>= signRepo keyname repo
        return release
      putIndex :: EnvPath -> Release -> PackageIndex -> [BinaryPackage] -> IO (Either [String] ())
      putIndex root release index packages =
                let text = formatControl (B.Control (List.map packageInfo packages)) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath release index) (L.fromChunks [encodeUtf8 (mconcat text)])

-- | Delete specific source packages and their associated binary packages.
deleteBinaryPackages :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Bool -> Maybe PGPKey -> Set (Release, PackageIndex, PackageID BinPkgName) -> m ()
deleteBinaryPackages _ _ s | Set.null s = return ()
deleteBinaryPackages dry keyname blacklist = do
  repo <- view repository <$> getInstall
  mapM_ doIndex (Set.toList (allIndexes repo))
    where
      doIndex (release, index) = getEntries release index >>= put release index . List.partition (victim release index)
      put :: (MonadIO m, MonadInstall s m) => Release -> PackageIndex -> ([BinaryPackage], [BinaryPackage]) -> m Release
      put release index ([], _) =
          qPutStrLn ("deleteBinaryPackages - nothing to remove from " ++ show index) >>
          return release
      put release index (junk, keep) =
          qPutStrLn ("deleteBinaryPackages - removing " ++ show (length junk) ++ " packages from " ++ ppShow (release, index) ++ ", leaving " ++ show (length keep) {- ++ ":\n " ++ intercalate "\n " (List.map (show . F.pretty . packageID) junk) -}) >>
          putIndex' keyname release index keep
      allIndexes repo = Set.fold Set.union Set.empty (Set.map (\ r -> Set.fromList (List.map (r,) (packageIndexes r))) (Set.fromList (view repoReleaseInfoLocal repo)))

      -- (invalid, indexes) = Set.partition (\ (_, i) -> packageIndexArch i == Source) (Set.fromList (List.map (\ (r, i, _) -> (r, i)) (toList packages)))
      -- (source, invalid) = Set.partition (\ (r, i, b) -> packageIndexArch i == Source) (Set.fromList packages)
      -- (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (List.map fst packages))
      -- allIndexes (release, sourceIndex) = packageIndexList release

      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: Release -> PackageIndex -> BinaryPackage -> Bool
      victim release index binaryPackage = Set.member (release, index, packageID binaryPackage) blacklist

      getEntries :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Release -> PackageIndex -> m [BinaryPackage]
      getEntries release index = getPackages_ release index >>= return . either (error . show) id
      putIndex' :: (MonadIO m, MonadInstall s m) => Maybe PGPKey -> Release -> PackageIndex -> [BinaryPackage] -> m Release
      putIndex' keyname release index entries =
          do repo <- view repository <$> getInstall
             case dry of
               True -> ePutStrLn ("dry run: not changing " ++ show index)
               False -> liftIO $ putIndex (view repoRoot repo) release index entries >> writeRelease repo release >>= signRepo keyname repo
             return release
      putIndex :: EnvPath -> Release -> PackageIndex -> [BinaryPackage] -> IO (Either [String] ())
      putIndex root release index packages =
                let text = formatControl (B.Control (List.map packageInfo packages)) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath release index) (L.fromChunks [encodeUtf8 (mconcat text)])

sourceIdent :: (Release, PackageIndex, BinaryPackage) -> (Release, PackageIndex, PackageID BinPkgName)
sourceIdent (release, index, entry) =
    case packageIndexArch index of
      Source -> (release, index, packageID entry)
      _ -> (release, (index {packageIndexArch = Source}), binaryPackageSourceID_ index entry)

{-
uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""
-}

sourceFilePaths_ :: SourcePackage -> Set FilePath
sourceFilePaths_ package =
    Set.map ((sourceDirectory package) </>) . Set.map sourceFileName . Set.fromList . sourcePackageFiles $ package

toSourcePackage_ :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage_ index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion' . T.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case (parseSourcesFileList files, parseSourceParagraph package) of
            (Right files', Right para) ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID (T.unpack name) version
                , sourceParagraph = package
                , sourceControl = para
                , sourceDirectory = T.unpack directory
                , sourcePackageFiles = files' }
            (Left messages, _) -> error $ "Invalid file list: " ++ show messages
            (_, Left messages) -> error $ "Error in source paragraph\n package=" ++ show package ++ "\n  index=" ++ show index ++ "\n  messages:\n   " ++ intercalate "\n   " messages
      x -> error $ "Missing info in source package control information in " ++ show index ++ " -> " ++ show x ++ " :\n" ++ T.unpack (formatParagraph package)
    where
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: T.Text -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . List.map parseSourcesFiles . lines . T.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . List.map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . List.map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Either [String] SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    case (B.fieldValue "Package" p,
          B.fieldValue "Maintainer" p) of
      (Just source', Just maintainer') ->
          -- The optional fields can be parsed as pure values
          Right (SourceControl
                  { source = source'
                  , maintainer = maintainer'
                  , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
                  , packageSection = fmap stripWS $ B.fieldValue "Section" p
                  , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
                  , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
                  , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
                  , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
                  , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
                  , standardsVersion = fmap stripWS $ B.fieldValue "Standards-Version" p
                  , homepage = fmap stripWS $ B.fieldValue "Homepage" p })
      _x -> Left ["parseSourceParagraph - One or more required fields (Package, Maintainer, Standards-Version) missing: " ++ show p]

toBinaryPackage_ :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage_ release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion' (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID_ :: PackageIndex -> BinaryPackage -> PackageID BinPkgName
binaryPackageSourceID_ (PackageIndex _component _) package =
    case maybe Nothing (matchRegex re . T.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> makeBinaryPackageID name (packageVersion pid)
      Just [name, _, version] -> makeBinaryPackageID name (parseDebianVersion' version)
      _ -> error "Missing Source attribute in binary package info"
    where
      -- sourceIndex = PackageIndex component Source
      pid = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryIDs_ :: SourcePackage -> [PackageID BinPkgName]
sourcePackageBinaryIDs_ package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> List.map (binaryID (parseDebianVersion' (T.unpack version))) $ splitRegex (mkRegex "[ ,]+") (T.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = makeBinaryPackageID name version
      -- binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

-- | Get the contents of a package index
getPackages_ :: forall s e m. (MonadInstall s m, MonadIOError e m, HasParseError e) => Release -> PackageIndex -> m (Either e [BinaryPackage])
getPackages_ release index = do
  repo <- view repository <$> getInstall
  let repoURI = repoKeyURI . repoKey $ repo
      packageIndexURI = over uriPathLens (\path -> path </> packageIndexPath release index) (view vendorURI repoURI)
  liftIOError (fileFromURIStrict Nothing packageIndexURI) >>= readControl packageIndexURI
    where
      readControl :: URI -> L.ByteString -> m (Either e [BinaryPackage])
      readControl uri s =
          case controlFromIndex Uncompressed (show uri) s of
            Left e -> return $ Left $ fromParseError e
            Right (B.Control control) -> return (Right $ List.map (toBinaryPackage_ release index) control)
          -- return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id

-- | Get the contents of a package index
binaryPackagesOfIndex_ :: (MonadIO m, MonadInstall s m, HasIOException e, HasParseError e, MonadError e m) => Release -> PackageIndex -> m (Either e [BinaryPackage])
binaryPackagesOfIndex_ release index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> getPackages_ release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex_ :: (MonadIO m, MonadInstall s m, HasIOException e, HasParseError e, MonadError e m) => Release -> PackageIndex -> m (Either e [SourcePackage])
sourcePackagesOfIndex_ release index =
    case packageIndexArch index of
      Source -> getPackages_ release index >>= return . either Left (Right . List.map (toSourcePackage_ index . packageInfo))
      _ -> return (Right [])

{-
indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> FilePath
indexPrefix repo release index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     codename distro ++ "_" ++ (sectionName' $ section))
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distro = releaseName $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s, []) -> [s]
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b
-}

-- | Return a list of all source packages.
releaseSourcePackages_ :: forall s e m. (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Release -> m (Set SourcePackage)
releaseSourcePackages_ release =
    mapM (sourcePackagesOfIndex_ release) (sourceIndexes release) >>= return . test
    where
      test :: [Either e [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages_ :: (MonadIO m, MonadInstall s m, Show e, HasIOException e, HasParseError e, MonadError e m) => Release -> m (Set BinaryPackage)
releaseBinaryPackages_ release =
    mapM (binaryPackagesOfIndex_ release) (binaryIndexes release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Write a set of packages into a package index.
putPackages_ :: LocalRepository -> Release -> PackageIndex ->  [BinaryPackage] -> IO ()
putPackages_ repo release index packages =
    writeAndZipFileWithBackup (outsidePath (view repoRoot repo) </> packageIndexPath release index) (L.fromChunks [encodeUtf8 text]) >>= either (fail . intercalate "\n") return
    where
      text = T.concat (intersperse (T.pack "\n") . List.map formatParagraph . List.map packageInfo $ packages)

{-
readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)                 -- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")     -- Debugging output
       return paragraphs
-}
