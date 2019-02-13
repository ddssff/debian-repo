{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, PackageImports, StandaloneDeriving, TemplateHaskell, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Slice
    ( Slice(..)
    , PPASlice(..)
    , SliceList(..)
    , NamedSliceList(..)
    , sourceSlices
    , binarySlices
    , inexactPathSlices
    , releaseSlices
    , appendSliceLists
    , UpdateError(..)
    , SourcesChangedAction(..)
    , doSourcesChangedAction
    , addAptRepository
    , removeAptRepository
    , expandPPASlice
    ) where

import Control.Exception (Exception)
import Control.Lens (over, view)
import Data.Data (Data)
import Data.List (intersperse)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Data.Text (Text)
import Data.Text.IO as Text (putStr, hPutStr)
import Data.Typeable (Typeable)
import Debian.Codename (Codename, codename)
import Debian.Repo.EnvPath (EnvPath(EnvPath, _envRoot), EnvRoot(EnvRoot))
import Debian.Repo.Prelude (replaceFile)
import Debian.Repo.Prelude.Verbosity (ePutStr, ePutStrLn)
import Debian.Repo.Repo (RepoKey(Remote, Local))
import Debian.Sources (DebSource(..), SourceType(Deb, DebSrc), parseSourcesList, sourceDist, sourceUri, sourceType)
import Debian.TH (here, Loc)
import Debian.URI (toURI', uriPathLens)
import Debian.VendorURI (vendorURI)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.IO (hGetLine, stdin, stderr)
import System.Process (proc)
import System.Process.ListLike (readCreateProcess, Chunk(..), collectOutput)
import System.Process.Text ()
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.HughesPJClass (hcat, text)
import Distribution.Pretty

data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)

instance Pretty Slice where
    pretty x@(Slice {sliceRepoKey = Local (EnvPath {_envRoot = EnvRoot root})}) = pretty $ over (sourceUri . vendorURI . uriPathLens) (root ++) (sliceSource x)
    pretty x@(Slice {}) = pretty $ sliceSource $ x

data PPASlice = PersonalPackageArchive {ppaUser :: String, ppaName :: String} deriving (Eq, Ord, Show)

instance Pretty PPASlice where
    pretty x@(PersonalPackageArchive {}) = text ("ppa:" ++ ppaUser x </> ppaName x)

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: Codename
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = hcat . intersperse (text "\n") . map pretty . slices

instance Pretty NamedSliceList where
    pretty = pretty . sliceList

sourceSlices :: SliceList -> SliceList
sourceSlices = SliceList . filter ((== DebSrc) . view sourceType . sliceSource) . slices

binarySlices :: SliceList -> SliceList
binarySlices = SliceList . filter ((== Deb) . view sourceType . sliceSource) . slices

inexactPathSlices :: SliceList -> SliceList
inexactPathSlices = SliceList . filter (either (const False) (const True) . view sourceDist . sliceSource) . slices

releaseSlices :: Codename -> SliceList -> SliceList
releaseSlices release list =
    SliceList . filter (isRelease . view sourceDist . sliceSource) $ (slices list)
    where isRelease = either (const False) (\ (x, _) -> x == release)

appendSliceLists :: [SliceList] -> SliceList
appendSliceLists lists =
    SliceList { slices = concat (map slices lists) }

{-
-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (Maybe EnvRoot) -> URI -> IO [Text]
uriSubdirs root uri =
    liftIO (dirFromURI uri') >>= either throw (return . map pack)
    where
      uri' = case uriScheme uri of
               "file:" -> uri {uriPath = maybe "" rootPath root ++ (uriPath uri)}
               _ -> uri

readRelease :: URI -> Text -> IO (Maybe (Paragraph' Text))
readRelease uri name =
    do output <- liftIO (fileFromURI uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri </> "dists" </> unpack name </> "Release"}
-}

data UpdateError
    = Changed Codename FilePath SliceList SliceList
    | Missing Codename FilePath
    | Flushed
    deriving Typeable

instance Exception UpdateError

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, prettyShow l1, prettyShow l2]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

doSourcesChangedAction :: [Loc] -> FilePath -> FilePath -> NamedSliceList -> SliceList -> SourcesChangedAction -> IO ()
doSourcesChangedAction locs dir sources baseSources fileSources SourcesChangedError = do
  ePutStrLn ("The sources.list in the existing '" ++ (codename . sliceListName $ baseSources) ++ "' in " ++ dir ++
             " apt-get environment doesn't match the parameters passed to the autobuilder" ++ ":\n\n" ++
             sources ++ ":\n\n" ++
             prettyShow fileSources ++
             "\n\nRun-time parameters:\n\n" ++
             prettyShow baseSources ++
             "\nIt is likely that the build environment in\n" ++
             dir ++ " is invalid and should be rebuilt.\n\n at " <> prettyShow ($here : locs))
  ePutStr $ "Remove it and continue (or exit)?  [y/n]: "
  result <- hGetLine stdin
  case result of
    ('y' : _) ->
        do removeRecursiveSafely dir
           createDirectoryIfMissing True dir
           replaceFile sources (prettyShow baseSources)
    _ -> error ("Please remove " ++ dir ++ " and restart.")

doSourcesChangedAction _locs dir sources baseSources _fileSources RemoveRelease = do
  ePutStrLn $ "Removing suspect environment: " ++ dir
  removeRecursiveSafely dir
  createDirectoryIfMissing True dir
  replaceFile sources (prettyShow baseSources)

doSourcesChangedAction _locs dir sources baseSources _fileSources UpdateSources = do
  -- The sources.list has changed, but it should be
  -- safe to update it.
  ePutStrLn $ "Updating environment with new sources.list: " ++ dir
  removeFile sources
  replaceFile sources (prettyShow baseSources)

addAptRepository :: Slice ->  IO (ExitCode, [Chunk Text])
addAptRepository slice = readCreateProcess (proc "add-apt-repository" ["--yes", "--enable-source", prettyShow slice]) mempty >>= mapM echoOutput >>= return . collectOutput
    where
      echoOutput :: Chunk Text -> IO (Chunk Text)
      echoOutput x@(Stdout s) = Text.putStr s >> return x
      echoOutput x@(Stderr s) = Text.hPutStr stderr s >> return x
      echoOutput x = return x

removeAptRepository :: Slice ->  IO (ExitCode, [Chunk Text])
removeAptRepository x = readCreateProcess (proc "add-apt-repository" ["--yes", "-r", prettyShow x]) mempty

{-
expandPPASlices' :: NamedSliceList -> [PPASlice] -> IO NamedSliceList
expandPPASlices' x ppaslices = do
  expanded <- mapM (expandPPASlice (relName (sliceListName x))) ppaslices >>= return . concat
  return $ x {sliceList = SliceList {slices = (slices (sliceList x)) ++ expanded}}

expandPPASlices :: String -> [PPASlice] -> IO [Slice]
expandPPASlices baseRepo ppaslices = mapM (expandPPASlice baseRepo) ppaslices >>= return . concat
-}

expandPPASlice :: Codename -> PPASlice -> IO [Slice]
expandPPASlice baseRepo x@(PersonalPackageArchive {}) = do
  debSources <- readFile ("/etc/apt/sources.list.d" </> ppaUser x ++ "-" ++ ppaName x ++ "-" ++ codename baseRepo ++ ".list") >>= return . parseSourcesList [$here]
  return $ map (\ ds -> Slice {sliceRepoKey = Remote (toURI' (view (sourceUri . vendorURI) ds)), sliceSource = ds}) debSources
