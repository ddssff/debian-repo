{-# LANGUAGE CPP, FlexibleContexts, PackageImports, RankNTypes, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.State.Slice
    ( verifySourcesList
    , repoSources
    , repoSources'
    , updateCacheSources
    ) where

import Control.Lens (over, review, view)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (toChunks)
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text as T (pack, Text, unpack)
import Debian.Control (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Control.Text (decodeParagraph)
import Debian.Codename (parseCodename)
import Debian.Pretty (prettyShow)
import Debian.Except (HasIOException, liftEIO, MonadIO, MonadError)
import Debian.Release (parseSection')
import Debian.Releases (DistsURI, distsURI, ReleaseTree, ReleaseURI, releaseURI)
import Debian.Repo.EnvPath (EnvPath(..), EnvRoot(..), outsidePath, rootPath)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Prelude (replaceFile, symbol)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Repo (repoKey)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(..), SourcesChangedAction, doSourcesChangedAction)
import Debian.Repo.State.Repository (readLocalRepository, prepareRemoteRepository)
import Debian.Repo.Top (MonadTop, distDir, sourcesPath)
import Debian.Repo.URI (dirFromURI, fileFromURI)
import Debian.Sources (DebSource(..), SourceOption(..), SourceType(Deb, DebSrc), parseSourcesList)
import Debian.TH (here)
import Debian.URI (parentURI, uriPathLens, uriSchemeLens)
import Debian.VendorURI (VendorURI, vendorURI)
import Language.Haskell.TH.Syntax (Loc)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Regex (mkRegex, splitRegex)

-- | Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources ::
    (MonadIO m, MonadRepos s m, HasIOException e, MonadError e m)
    => [Loc] -> Maybe EnvRoot -> [SourceOption] -> VendorURI -> m SliceList
repoSources locs chroot opts venuri =
    do let distsuri :: DistsURI
           distsuri = review distsURI (over uriPathLens (</> "dists") (view vendorURI venuri))
       -- review distsURI $ parentURI $ view releaseURI reluri
       dirs <- liftIO $ uriSubdirs ($here : locs) chroot distsuri
       let reluris = fmap (\dir -> review releaseURI (over uriPathLens (</> (unpack dir <> "/")) (view distsURI distsuri))) dirs
       releaseFiles <- catMaybes <$> mapM (readRelease ($here : locs) chroot) reluris
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      -- venuri = review vendorURI $ parentURI $ parentURI $ view releaseURI reluri
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseCodename (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseCodename (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

releaseSources :: (MonadIO m, MonadRepos s m, HasIOException e, MonadError e m) => [Loc] -> Maybe EnvRoot -> [SourceOption] -> ReleaseURI -> m SliceList
releaseSources locs chroot opts reluri =
    do mreleaseFile <- readRelease ($here : locs) chroot reluri
       case mreleaseFile of
         Nothing -> error ("releaseSources - no Release file at " ++ show reluri)
         Just releaseFile -> do
           let codename = fieldValue "Codename" releaseFile
               section = maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components" $ releaseFile
               result = sources (codename, section)
           mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      venuri = (review vendorURI . parentURI . parentURI) (view releaseURI reluri)
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseCodename (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseCodename (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

repoSources' ::
    (MonadIO m, MonadRepos s m, Show e, HasIOException e, MonadError e m)
    => [Loc]
    -> (ReleaseTree -> Either e ReleaseURI)
    -> Maybe EnvRoot
    -> [SourceOption]
    -> ReleaseTree
    -> m SliceList
repoSources' locs myUploadURI chroot opts r = do
  case myUploadURI r of
    Right uri -> releaseSources ($here : locs) chroot opts uri
    Left e -> error ("repoSources' " ++ show e)

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (HasIOException e, MonadError e m, MonadIO m) => [Loc] -> Maybe EnvRoot -> DistsURI -> m [Text]
uriSubdirs locs root uri = do
  -- liftIO (putStrLn (prettyShow locs "- uriSubdirs root=" ++ show root ++ " uri=" ++ showURI (view distsURI uri)))
  map pack <$> liftEIO ($here : locs) (dirFromURI ($here : locs) (view distsURI uri'))
    where
      uri' = case view (distsURI . uriSchemeLens) uri of
               "file:" -> over (distsURI . uriPathLens) (maybe "" (view rootPath) root ++) uri
               _ -> uri

readRelease ::
    (HasIOException e, MonadIO m, MonadError e m)
    => [Loc]
    -> Maybe EnvRoot
    -> ReleaseURI
    -- -> Text -- always "Release"
    -> m (Maybe (Paragraph' Text))
readRelease locs chroot uri =
    do output <- fileFromURI ($here : locs) chroot uri'
       case output of
         s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                _ -> return Nothing
    where
      uri' = over uriPathLens (</> "Release") (view releaseURI uri)

#if 0
readRelease' :: ReleaseTree -> Text -> IO (Maybe (Paragraph' Text))
readRelease' r name =
    do output <- liftIO (fileFromURI $here releaseFile)
       case output of
         Left e -> liftIO (putStrLn ("Debian.Repo.State.Slice.readRelease " ++ show releaseFile ++ " -> " ++ show e)) >> return Nothing
         Right s -> case parseControl (show releaseFile) (B.concat . L.toChunks $ s) of
                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                      _ -> return Nothing
    where
      Right uri = myDownloadURI $here r
      releaseFile :: URI
      releaseFile = over uriPathLens (</> (unpack name </> "Release")) (view releaseURI uri)
#endif

-- | Make sure all the required local and remote repository objects
-- used by a sources.list file are in our cache.
verifySourcesList :: (MonadIO m, MonadRepos s m) => Maybe EnvRoot -> [DebSource] -> m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

verifyDebSource :: (MonadIO m, MonadRepos s m) => Maybe EnvRoot -> DebSource -> m Slice
verifyDebSource chroot line =
    case view (vendorURI . uriSchemeLens) (sourceUri line) of
      "file:" -> let path = EnvPath chroot' (view (vendorURI . uriPathLens) (sourceUri line)) in readLocalRepository path Nothing >>= maybe (error $ "No repository at " ++ show (outsidePath path)) (\ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line})
      _ -> prepareRemoteRepository (sourceUri line) >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
    where
      chroot' = fromMaybe (EnvRoot "") chroot

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.  (FIXME: Does this really work for MonadOS?)
updateCacheSources :: (MonadIO m, MonadRepos s m, MonadTop r m) => SourcesChangedAction -> NamedSliceList -> m ()
updateCacheSources sourcesChangedAction baseSources = do
  let rel = sliceListName baseSources
  dir <- distDir rel
  sources <- sourcesPath rel
  distExists <- liftIO $ doesFileExist sources
  case distExists of
    True -> do
      fileSources <- liftIO (readFile sources) >>= verifySourcesList Nothing . parseSourcesList [$here]
      when (fileSources /= sliceList baseSources)
           (qPutStrLn ($(symbol 'updateCacheSources) ++ " for " <> show rel) >>
            liftIO (doSourcesChangedAction dir sources baseSources fileSources sourcesChangedAction))
    False -> do
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ replaceFile sources (prettyShow baseSources)
  return ()
