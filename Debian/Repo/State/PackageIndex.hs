{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, PackageImports, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Debian.Repo.State.PackageIndex
    ( binaryPackagesFromSources
    , sourcePackagesFromSources
    ) where

import Control.Exception (throw, try)
import Control.Lens (view)
import Control.Monad.Except (MonadIO(liftIO))
import Data.Either (partitionEithers)
import Data.List as List (intercalate, map, partition)
import Data.Maybe (catMaybes)
import qualified Data.Text as T (Text, unpack)
import Debian.Arch (Arch, Arch(..), prettyArch)
import Debian.Codename (Codename, codename)
import Debian.Control (ControlFunctions(stripWS), formatParagraph)
import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(lookupP), ControlFunctions(parseControlFromHandle), Field, Field'(Field), fieldValue, Paragraph)
import Debian.Pretty (prettyShow)
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (sectionName')
import Debian.Releases (HasBaseRelease(baseRelease, baseReleaseString), parseReleaseTree)
import Debian.Repo.EnvPath (EnvRoot, rootPath)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID)
import Debian.Repo.PackageIndex (BinaryPackage, BinaryPackage(..), PackageIndex(..), PackageIndex(packageIndexArch, packageIndexComponent), packageIndexPath, SourceControl(..), SourceFileSpec(SourceFileSpec), SourcePackage(..), SourcePackage(sourcePackageID))
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (Release(releaseName, releaseAliases))
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey, repoKeyURI)
import Debian.Repo.Slice (binarySlices, Slice(sliceRepoKey, sliceSource), SliceList(slices), sourceSlices)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Sources (DebSource(_sourceDist, _sourceType), SourceType(Deb, DebSrc))
import Debian.TH (here)
import Debian.URI (uriSchemeLens, uriToString', uriAuthorityLens, uriPathLens)
import Debian.VendorURI (vendorURI)
import Debian.Version (parseDebianVersion')
import Extra.Except -- (HasIOException)
import Network.URI (escapeURIString, URIAuth(uriPort, uriRegName, uriUserInfo))
import qualified System.IO as IO (Handle, hClose, IOMode(ReadMode), openBinaryFile)
--import System.IO.Unsafe (unsafeInterleaveIO)
--import System.Process.Progress (qBracket, quieter)

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: (MonadRepos s m, MonadIOError e m, HasLoc e) => Arch -> Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes arch slice =
    foldRepository [$here] f f (sliceRepoKey slice)
    where
      f repo =
          case (_sourceDist (sliceSource slice)) of
            Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
            Right (release, sections) -> return $ map (makeIndex repo release) sections
      makeIndex repo release section =
          (repoKey repo,
           findReleaseInfo repo release,
           PackageIndex { packageIndexComponent = section
                        , packageIndexArch = case (_sourceType (sliceSource slice)) of
                                               DebSrc -> Source
                                               Deb -> arch })
      findReleaseInfo repo release =
          case filter (isReleaseName release) (repoReleaseInfo repo) of
            (x : _) -> x
            [] -> error $ ("sliceIndexes: Invalid release name: " ++ codename release ++
                           "\n  You may need to remove ~/.autobuilder/repoCache." ++
                           "\n  Available: " ++ (show . map releaseName . repoReleaseInfo $ repo)) ++
                           "\n repoKey: " ++ show (repoKey repo) ++
                           "\n repoReleaseInfo: " ++ show (repoReleaseInfo repo) ++
                           "\n slice: " ++ show slice
            -- xs -> error $ "Internal error 5 - multiple releases named " ++ codename release ++ "\n" ++ show xs

isReleaseName :: Codename -> Release -> Bool
isReleaseName name release = elem name (releaseName release : releaseAliases release)

data UpdateError
    = Changed Codename FilePath SliceList SliceList
    | Missing Codename FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, prettyShow l1, prettyShow l2]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

sourcePackagesFromSources ::
    (MonadRepos s m, MonadIOError e m, HasLoc e)
    => EnvRoot
    -> Arch
    -> SliceList
    -> m [SourcePackage]
sourcePackagesFromSources root arch sources = do
  indexes <- concat <$> (mapM (sliceIndexes arch) (slices . sourceSlices $ sources))
  packages <- mapM (uncurry3 (sourcePackagesOfIndex root arch)) indexes
  case partitionEithers packages of
    ([], xss) -> return $ concat xss
    -- This can be used for debugging, but in production package
    -- indexes may sometimes not exist.
    -- (es, _) -> error $ unlines $ "Debian.Repo.State.PackageIndex.sourcePackagesFromSources: Some package indexes could not be read:" : map show es
    (_, xss) -> return $ concat xss

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

-- FIXME: assuming the index is part of the cache
sourcePackagesOfIndex ::
    (MonadIO m)
    => EnvRoot
    -> Arch
    -> RepoKey
    -> Release
    -> PackageIndex
    -> m (Either (EnvRoot, Arch, RepoKey, Release, PackageIndex, [IOError]) [SourcePackage])
sourcePackagesOfIndex root arch repo release index = do
    qPutStrLn (prettyShow $here ++ " sourcePackagesOfIndex paths=" ++ show paths)
    -- quieter 2 $ qBracket ($(symbol 'sourcePackagesOfIndex) ++ " " ++ path) $
    -- unsafeInterleaveIO makes the package index file reads
    -- asynchronous, not sure what the performance implications
    -- are.  Anyway, this is now only called on demand, so the
    -- unsafeInterleaveIO is probably moot.
    either (Left . makeLeft)
           ((\paras -> Right (List.map (toSourcePackage index) paras))) <$> (liftIO (readParagraphs paths))
    where
      makeLeft :: [IOError] -> (EnvRoot, Arch, RepoKey, Release, PackageIndex, [IOError])
      makeLeft es = (root, arch, repo, release, index, es)
      paths = fmap ((view rootPath root) ++) suffs
      suffs = indexCacheFile arch repo release index

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
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

binaryPackagesFromSources :: (MonadRepos s m, MonadIOError e m, HasLoc e) => EnvRoot -> Arch -> SliceList -> m [BinaryPackage]
binaryPackagesFromSources root arch sources = do
  indexes <- mapM (sliceIndexes arch) (slices . binarySlices $ sources) >>= return . concat
  concat <$> (mapM (\ (repo, rel, index) -> either (const []) id <$> (binaryPackagesOfIndex root arch repo rel index)) indexes)

-- FIXME: assuming the index is part of the cache
binaryPackagesOfIndex :: (MonadIO m) => EnvRoot -> Arch -> RepoKey -> Release -> PackageIndex -> m (Either [IOError] [BinaryPackage])
binaryPackagesOfIndex root arch repo release index =
    either Left ((\paras -> Right (List.map (toBinaryPackage release index) paras))) <$> (liftIO (readParagraphs paths))
{-
    do paragraphs <- liftIO $ {-unsafeInterleaveIO-} (readParagraphs path)
       let packages = List.map (toBinaryPackage release index) paragraphs
       return packages
-}
    where
       suffs = indexCacheFile arch repo release index
       paths = fmap ((view rootPath root) ++) suffs

toBinaryPackage :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
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

-- | Try each path until one works
readParagraphs :: [FilePath] -> IO (Either [IOError] [B.Paragraph])
readParagraphs paths = go [] paths
    where
      go es (path : more) = readParagraphs' path >>= either (\e -> go (e : es) more) (return . Right)
      go es  [] = return $ Left es

readParagraphs' :: FilePath -> IO (Either IOError [B.Paragraph])
readParagraphs' path =
    try (IO.openBinaryFile path IO.ReadMode >>= go)
    where
      go :: IO.Handle -> IO [B.Paragraph]
      go h = do
        B.Control paragraphs <- either (throw . userError . show) id <$> B.parseControlFromHandle path h
        IO.hClose h
        return paragraphs

-- | Try all the release names
indexCacheFile :: Arch -> RepoKey -> Release -> PackageIndex -> [FilePath]
indexCacheFile arch repo release index =
    case (arch, packageIndexArch index) of
      (Binary _ _, Source) -> fmap (++ "_source_Sources") (indexPrefix repo release index)
      (Binary _ _, indexArch@(Binary _ _)) -> fmap (++ ("_binary-" ++ show (prettyArch indexArch) ++ "_Packages")) (indexPrefix repo release index)
      (x, _) -> error $ "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> [FilePath]
indexPrefix repo release index =
    fmap (\distro ->
              (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
               codename distro ++ "_" ++ (sectionName' $ section))) distros
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distros :: [Codename]
      distros = (releaseName release : releaseAliases release)
      scheme = view (vendorURI . uriSchemeLens) uri
      auth = view (vendorURI . uriAuthorityLens) uri
      path = view (vendorURI . uriPathLens) uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user'') Nothing (Just host) port' path' =
          case codename (releaseName release) of
            "artful-seereason-private" -> host ++ port' ++ escape path'
            "xenial-seereason-private" -> host ++ port' ++ escape path'
            _ -> aptUserPrefix release user'' ++ host ++ port' ++ escape path'
      prefix "http:" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix "ftp:" _ _ (Just host) _ path' =
          host ++ escape path'
      prefix "file:" Nothing Nothing Nothing "" path' =
          escape path'
      prefix "ssh:" (Just user'') Nothing (Just host) port' path' =
          case codename (releaseName release) of
            "artful-seereason-private" -> host ++ port' ++ escape path'
            "xenial-seereason-private" -> host ++ port' ++ escape path'
            _ -> aptUserPrefix release user'' ++ host ++ port' ++ escape path'
      prefix "ssh" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . view vendorURI . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s', []) -> [s']
            (h, t) -> h : wordsBy p (drop 1 t)

-- If an older version of apt downloads a source like
--    ssh://upload@deb.seereason.com ...
-- The resulting file in /var/apt/lists will be named
--    upload%40deb.seereason.com_srv_ ...
-- whereas with a newer apt (xenial or later?) the name is
--    deb.seereason.com_ ...

aptUserPrefix :: Release -> String -> String
aptUserPrefix r s =
    case baseReleaseString (baseRelease (parseReleaseTree (releaseName r))) of
      "trusty" -> s
      _ -> ""

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b
