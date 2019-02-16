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

import Control.Lens (over, review, set, view)
--import Control.Monad (when)
import Control.Monad.Except (liftIO)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (readFile, toChunks)
import Data.List (nub, nubBy)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Text as T (pack, Text, unpack)
import Debian.Control (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Control.Text (decodeParagraph)
import Debian.Codename (parseCodename)
import Debian.Pretty (prettyShow)
import Debian.Release (parseSection')
import Debian.Releases (ReleaseTree, ReleaseURI, releaseURI)
import Debian.Repo.EnvPath (EnvPath(..), EnvRoot(..), outsidePath)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Prelude (replaceFile)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Repo (RepoKey(Local), repoKey)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(..), SourcesChangedAction, doSourcesChangedAction)
import Debian.Repo.State.Repository (readLocalRepository, prepareRemoteRepository)
import Debian.Repo.Top (MonadTop, distDir, sourcesPath)
import Debian.Repo.URI (fileFromURI)
import Debian.Sources (DebSource(..), parseSourcesList, SourceOption(..), SourceType(Deb, DebSrc), sourceUri)
import Debian.TH (here)
import Debian.URI (parentURI, parseURI, uriPathLens, uriSchemeLens)
import Debian.VendorURI (VendorURI, vendorURI)
import Extra.Except -- (HasIOException(fromIOException), liftIOError, MonadIO, MonadError, throwError)
import Language.Haskell.TH.Syntax (Loc)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), dropDrive, makeRelative)
import Text.Regex (mkRegex, splitRegex)

-- | Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources ::
    (MonadRepos s m, MonadIOError e m, HasLoc e)
    => [Loc] -> [SourceOption] -> EnvPath -> m SliceList
repoSources locs opts venpath =
    do --qPutStrLn ("repoSources - venpath=" <> show venpath <> " at " <> prettyShow ($here : locs))
       dirs <- liftIO $ listDirectory (outsidePath venpath </> "dists")
       --ePutStrLn ("repoSources - dirs=" ++ show dirs ++ " at " <> prettyShow ($here : locs))
       let relpaths = fmap (\dir -> outsidePath venpath </> "dists" </> dir) dirs
       --ePutStrLn ("repoSources - relpaths=" ++ show relpaths ++ " at " <> prettyShow ($here : locs))
       (releaseFiles :: [Paragraph' Text]) <- catMaybes <$> mapM readReleaseFromPath (fmap (</> "Release") relpaths)
       --ePutStrLn ("repoSources - releaseFiles=" ++ show releaseFiles ++ " at " <> prettyShow ($here : locs))
       let suites = map (maybe Nothing (zap (flip elem (fmap pack dirs)))) . map (fieldValue "Suite") $ releaseFiles
           --codenames = map (maybe Nothing (zap (flip elem (fmap pack dirs)))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip {-codenames-} suites $ sections
       mapM (verifyDebSource ($here : locs) (Just (_envRoot venpath))) result >>= (\ list -> return $ SliceList { slices = list })
    where
      venuri :: VendorURI
      venuri = review vendorURI (fromJust (parseURI ("file://" <> _envPath venpath))) -- review vendorURI $ parentURI $ parentURI $ view releaseURI reluri
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {_sourceType = Deb, _sourceOptions = opts, _sourceUri = venuri, _sourceDist = Right (parseCodename (unpack codename), components)},
           DebSource {_sourceType = DebSrc, _sourceOptions = opts, _sourceUri = venuri, _sourceDist = Right (parseCodename (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

releaseSources ::
    (MonadRepos s m, MonadIOError e m, HasLoc e)
    => [Loc] -> Maybe EnvRoot -> [SourceOption] -> ReleaseURI -> m SliceList
releaseSources locs chroot opts reluri =
    do mreleaseFile <- readRelease chroot reluri
       case mreleaseFile of
         Nothing -> error ("releaseSources - no Release file at " ++ show reluri)
         Just releaseFile -> do
           let codename = fieldValue "Codename" releaseFile
               section = maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components" $ releaseFile
               result = sources (codename, section)
           mapM (verifyDebSource ($here : locs) Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      venuri = (review vendorURI . parentURI . parentURI) (view releaseURI reluri)
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {_sourceType = Deb, _sourceOptions = opts, _sourceUri = venuri, _sourceDist = Right (parseCodename (unpack codename), components)},
           DebSource {_sourceType = DebSrc, _sourceOptions = opts, _sourceUri = venuri, _sourceDist = Right (parseCodename (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      --zap p x = if p x then Just x else Nothing

repoSources' ::
    (MonadRepos s m, Show e, MonadIOError e m, HasLoc e)
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

#if 0
-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (MonadIOError e m, HasLoc e) => [Loc] -> Maybe EnvRoot -> DistsURI -> m [Text]
uriSubdirs locs root uri = do
  -- liftIO (putStrLn (prettyShow locs "- uriSubdirs root=" ++ show root ++ " uri=" ++ showURI (view distsURI uri)))
  map pack <$> liftIOError ($here : locs) (dirFromURI ($here : locs) (view distsURI uri'))
    where
      uri' = case view (distsURI . uriSchemeLens) uri of
               "file:" -> over (distsURI . uriPathLens) (maybe "" (view rootPath) root ++) uri
               _ -> uri
#endif

readRelease ::
    (MonadIOError e m)
    => Maybe EnvRoot
    -> ReleaseURI
    -- -> Text -- always "Release"
    -> m (Maybe (Paragraph' Text))
readRelease chroot uri =
    do output <- liftIOError $ fileFromURI chroot uri'
       case output of
         s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                _ -> return Nothing
    where
      uri' = over uriPathLens (</> "Release") (view releaseURI uri)

readReleaseFromPath ::
    (HasIOException e, MonadIO m, MonadError e m)
    => FilePath
    -> m (Maybe (Paragraph' Text))
readReleaseFromPath path =
    do output <- liftIOError $ L.readFile path
       case output of
         s -> case parseControl path (B.concat . L.toChunks $ s) of
                Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                _ -> return Nothing
      -- where uri' = over uriPathLens (</> "Release") (view releaseURI uri)

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
verifySourcesList ::
    (MonadRepos s m, MonadIOError e m, HasLoc e)
    => [Loc]
    -> Maybe EnvRoot
    -- ^ If the url in any Slice is local and has this envroot as
    -- prefix split it out into the RepoKey.
    -> [DebSource]
    -> m SliceList
verifySourcesList locs chroot list =
    mapM (verifyDebSource ($here : locs) chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

-- | If the url in line is a file: url try to split the chroot prefix
-- out of it and use it to build the EnvPath in sliceRepoKey.
verifyDebSource ::
    (MonadIOError e m, HasLoc e, MonadRepos s m)
    => [Loc]
    -> Maybe EnvRoot
    -> DebSource
    -> m Slice
verifyDebSource locs chroot line = do
    --qPutStrLn ("verifyDebSource - chroot=" ++ show chroot ++ " line=" ++ show line ++ " at " <> prettyShow ($here : locs))
    case view (sourceUri . vendorURI . uriSchemeLens) line of
      "file:" -> do
        -- We have now split the uri into a chroot path and an inside path
        let (epath :: EnvPath) = toEnvPath chroot (view (sourceUri . vendorURI . uriPathLens) line)
            line' = set (sourceUri . vendorURI . uriPathLens) (_envPath epath) line
        --ePutStrLn ("verifyDebSource - epath=" ++ show epath)
        mrepo <- readLocalRepository ($here : locs) epath Nothing
        --ePutStrLn ("verifyDebSource - mrepo=" ++ show mrepo)
        maybe --(qPutStrLn ("No repository at " ++ show path ++ ", ignore and continue"))
              (throwError $ fromIOException $ userError $ "No repository at " ++ show epath)
              (\repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line'})
              mrepo
      _ -> withError (withLoc $here) $ prepareRemoteRepository ($here : locs) (_sourceUri line) >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
    --where
      --chroot' :: EnvRoot
      --chroot' = fromMaybe (EnvRoot "") chroot

-- toEnvPath (Just (EnvRoot "/")) path -> path
-- toEnvPath (Just (EnvRoot "/srv/dists/bionic/clean")) "/srv/dists/bionic/clean/work/localpool"
--   -> EnvPath (EnvRoot "/srv/dists/bionic/clean") "/work/localpool"
toEnvPath :: Maybe EnvRoot -> FilePath -> EnvPath
toEnvPath Nothing path = EnvPath (EnvRoot "/") path
toEnvPath (Just (root@(EnvRoot pref))) path =
    EnvPath root path'
    -- We need this to start with exactly one /, this seems to do it.
    where path' = "/" <> dropDrive (makeRelative pref path)

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.  (FIXME: Does this really work for MonadOS?)

-- data NamedSliceList = NamedSliceList { sliceList :: SliceList, sliceListName :: Codename }
-- data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)
-- data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)
-- data RepoKey = Remote URI' | Local EnvPath

-- | Find and return the 'EnvRoot' of the 'Local' lines of a
-- 'NamedSliceList'.
findEnvRoot :: forall e m. (MonadIOError e m, HasLoc e) => [Loc] -> NamedSliceList -> m (Maybe EnvRoot)
findEnvRoot locs nsl =
    case nub (mapMaybe (repoKeyPath . sliceRepoKey) (slices (sliceList nsl))) of
      [] -> return Nothing
      [r] -> return $ Just r
      rs -> withError (withLoc $here) $ throwError (fromIOException (userError ("Multiple distinct roots in local sources: " ++ show rs ++ " at " ++ prettyShow ($here : locs))))
    where repoKeyPath (Local p) = Just (_envRoot p)
          repoKeyPath _ = Nothing

updateCacheSources ::
    (MonadRepos s m, MonadTop r m, MonadIOError e m, HasLoc e)
    => [Loc] -> SourcesChangedAction -> NamedSliceList -> m ()
updateCacheSources locs sourcesChangedAction baseSources = do
  let rel = sliceListName baseSources
  chroot <- findEnvRoot ($here : locs) baseSources
  dir <- distDir rel
  sources <- sourcesPath rel
  qPutStrLn ("updateCacheSources - sources=" ++ show sources ++ " chroot=" ++ show chroot ++ " at " <> prettyShow ($here : locs))
  distExists <- liftIO $ doesFileExist sources
  case distExists of
    True -> do
      (fileSources :: Either e SliceList) <- tryError (liftIOError (readFile sources) >>= verifySourcesList ($here : locs) chroot . parseSourcesList [$here])
      case fileSources of
        Right s
          | s /= sliceList baseSources -> do
             --qPutStrLn ("updateCacheSources - s=" ++ show s ++ " sliceList baseSources=" ++ show (sliceList baseSources) ++ " for " <> show rel ++ " at " ++ prettyShow ($here : locs))
             withError (withLoc $here) $ liftIOError (doSourcesChangedAction ($here : locs) dir sources baseSources s sourcesChangedAction)
        _ -> return ()
    False -> do
      withError (withLoc $here) $ liftIOError $ createDirectoryIfMissing True dir
      withError (withLoc $here) $ liftIOError $ replaceFile sources (prettyShow baseSources)
  return ()
