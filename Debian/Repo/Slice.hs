{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, PackageImports, StandaloneDeriving, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Slice
    ( Slice(..)
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
    ) where

import Control.Exception (Exception)
import Data.Data (Data)
import Data.List (intersperse)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Text.IO as Text (putStr, hPutStr)
import Data.Typeable (Typeable)
import Debian.Release (ReleaseName(relName))
import Debian.Repo.Prelude (replaceFile)
import Debian.Repo.Prelude.Verbosity (ePutStr, ePutStrLn)
import Debian.Repo.Repo (RepoKey)
import Debian.Sources (DebSource(..), SliceName, SourceType(..))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.IO (hGetLine, stdin, hPutStr, stderr)
import System.Process (proc)
import System.Process.ListLike (readCreateProcess, Chunk(..), collectOutput)
import System.Process.Text ()
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), hcat, text, prettyShow)

data Slice
    = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource}
    | PersonalPackageArchive {ppaUser :: String, ppaName :: String}
    deriving (Eq, Ord, Show)

instance Pretty Slice where
    pPrint x@(PersonalPackageArchive {}) = text ("ppa:" ++ ppaUser x </> ppaName x)
    pPrint x@(Slice {}) = text ("ppa:" ++ ppaUser x </> ppaName x)

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pPrint = hcat . intersperse (text "\n") . map (pPrint . sliceSource) . slices

instance Pretty NamedSliceList where
    pPrint = pPrint . sliceList

deriving instance Show SourceType
deriving instance Show DebSource

sourceSlices :: SliceList -> SliceList
sourceSlices = SliceList . filter ((== DebSrc) . sourceType . sliceSource) . slices

binarySlices :: SliceList -> SliceList
binarySlices = SliceList . filter ((== Deb) . sourceType . sliceSource) . slices

inexactPathSlices :: SliceList -> SliceList
inexactPathSlices = SliceList . filter (either (const False) (const True) . sourceDist . sliceSource) . slices

releaseSlices :: ReleaseName -> SliceList -> SliceList
releaseSlices release list =
    SliceList . filter (isRelease . sourceDist . sliceSource) $ (slices list)
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
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
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

doSourcesChangedAction :: FilePath -> FilePath -> NamedSliceList -> SliceList -> SourcesChangedAction -> IO ()
doSourcesChangedAction dir sources baseSources fileSources SourcesChangedError = do
  ePutStrLn ("The sources.list in the existing '" ++ (relName . sliceListName $ baseSources) ++ "' in " ++ dir ++
             " apt-get environment doesn't match the parameters passed to the autobuilder" ++ ":\n\n" ++
             sources ++ ":\n\n" ++
             prettyShow fileSources ++
	     "\nRun-time parameters:\n\n" ++
             prettyShow baseSources ++ "\n" ++
	     "It is likely that the build environment in\n" ++
             dir ++ " is invalid and should be rebuilt.")
  ePutStr $ "Remove it and continue (or exit)?  [y/n]: "
  result <- hGetLine stdin
  case result of
    ('y' : _) ->
        do removeRecursiveSafely dir
           createDirectoryIfMissing True dir
           replaceFile sources (prettyShow baseSources)
    _ -> error ("Please remove " ++ dir ++ " and restart.")

doSourcesChangedAction dir sources baseSources _fileSources RemoveRelease = do
  ePutStrLn $ "Removing suspect environment: " ++ dir
  removeRecursiveSafely dir
  createDirectoryIfMissing True dir
  replaceFile sources (prettyShow baseSources)

doSourcesChangedAction dir sources baseSources _fileSources UpdateSources = do
  -- The sources.list has changed, but it should be
  -- safe to update it.
  ePutStrLn $ "Updating environment with new sources.list: " ++ dir
  removeFile sources
  replaceFile sources (prettyShow baseSources)

addAptRepository :: Slice ->  IO (ExitCode, [Chunk Text])
addAptRepository x = readCreateProcess (proc "add-apt-repository" ["--yes", "--enable-source", prettyShow x]) mempty >>= mapM echoOutput >>= return . collectOutput
    where
      echoOutput :: Chunk Text -> IO (Chunk Text)
      echoOutput x@(Stdout s) = Text.putStr s >> return x
      echoOutput x@(Stderr s) = Text.hPutStr stderr s >> return x
      echoOutput x = return x

removeAptRepository :: Slice ->  IO (ExitCode, [Chunk Text])
removeAptRepository x = readCreateProcess (proc "add-apt-repository" ["--yes", "-r", prettyShow x]) mempty
