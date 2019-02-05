{-# LANGUAGE CPP, FlexibleContexts, PackageImports, RankNTypes, TemplateHaskell, TupleSections #-}
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

import Control.Exception (throw)
import Control.Lens (over, review, view)
import Control.Monad (when)
--import Control.Monad.Except (MonadError)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (toChunks)
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text as T (pack, Text, unpack)
import Debian.AutoBuilder.Details.Sources ({-myDownloadURI,-} myUploadURI')
import Debian.Control (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Control.Text (decodeParagraph)
import Debian.Pretty (prettyShow)
import Debian.Release (parseReleaseName, parseSection')
import Debian.Releases (DistsURI, distsURI, ReleaseTree, ReleaseURI, releaseURI)
import Debian.Repo.EnvPath (EnvPath(..), EnvRoot(..), outsidePath, rootPath)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Prelude (replaceFile, symbol)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Repo (repoKey)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(..), SourcesChangedAction, doSourcesChangedAction)
import Debian.Repo.State.Repository (readLocalRepository, prepareRemoteRepository)
import Debian.Repo.Top (MonadTop, distDir, sourcesPath)
import Debian.Sources (DebSource(..), SourceOption(..), SourceType(Deb, DebSrc), parseSourcesList, vendorURI)
import Debian.TH (here)
import Debian.URI (dirFromURI, fileFromURI, parentURI, uriPathLens, uriSchemeLens)
import Language.Haskell.TH.Syntax (Loc)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Regex (mkRegex, splitRegex)

-- | Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources :: MonadRepos s m => Maybe EnvRoot -> [SourceOption] -> ReleaseURI -> m SliceList
repoSources chroot opts reluri =
    do dirs <- liftIO $ uriSubdirs $here chroot $ review distsURI $ parentURI $ view releaseURI reluri
       releaseFiles <- mapM (liftIO . readRelease reluri) dirs >>= return . catMaybes
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      venuri = review vendorURI $ parentURI $ parentURI $ view releaseURI reluri
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseReleaseName (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceOptions = opts, sourceUri = venuri, sourceDist = Right (parseReleaseName (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

repoSources' ::
    (MonadRepos s m)
    => Maybe EnvRoot
    -> [SourceOption]
    -> ReleaseTree
    -> m SliceList
repoSources' chroot opts r = do
  case myUploadURI' $here r of
    Right uri -> repoSources chroot opts uri
    Left e -> error ("repoSources' " ++ show e)

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: Loc -> Maybe EnvRoot -> DistsURI -> IO [Text]
uriSubdirs loc root uri = do
  -- liftIO (putStrLn (prettyShow loc "- uriSubdirs root=" ++ show root ++ " uri=" ++ showURI (view distsURI uri)))
  liftIO (dirFromURI loc (view distsURI uri')) >>= either throw (return . map pack)
    where
      uri' = case view (distsURI . uriSchemeLens) uri of
               "file:" -> over (distsURI . uriPathLens) (maybe "" (view rootPath) root ++) uri
               _ -> uri

readRelease :: ReleaseURI -> Text -> IO (Maybe (Paragraph' Text))
readRelease uri name =
    do output <- liftIO (fileFromURI $here uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
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
verifySourcesList :: MonadRepos s m => Maybe EnvRoot -> [DebSource] -> m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

verifyDebSource :: MonadRepos s m => Maybe EnvRoot -> DebSource -> m Slice
verifyDebSource chroot line =
    case view (vendorURI . uriSchemeLens) (sourceUri line) of
      "file:" -> let path = EnvPath chroot' (view (vendorURI . uriPathLens) (sourceUri line)) in readLocalRepository path Nothing >>= maybe (error $ "No repository at " ++ show (outsidePath path)) (\ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line})
      _ -> prepareRemoteRepository (sourceUri line) >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
    where
      chroot' = fromMaybe (EnvRoot "") chroot

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.  (FIXME: Does this really work for MonadOS?)
updateCacheSources :: (MonadRepos s m, MonadTop r m) => SourcesChangedAction -> NamedSliceList -> m ()
updateCacheSources sourcesChangedAction baseSources = do
  let rel = sliceListName baseSources
  dir <- distDir rel
  sources <- sourcesPath rel
  distExists <- liftIO $ doesFileExist sources
  case distExists of
    True -> do
      fileSources <- liftIO (readFile sources) >>= verifySourcesList Nothing . parseSourcesList $here
      when (fileSources /= sliceList baseSources)
           (qPutStrLn ($(symbol 'updateCacheSources) ++ " for " <> show rel) >>
            liftIO (doSourcesChangedAction dir sources baseSources fileSources sourcesChangedAction))
    False -> do
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ replaceFile sources (prettyShow baseSources)
  return ()
