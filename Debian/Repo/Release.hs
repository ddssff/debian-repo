-- | A release is a named collection of package indexes, e.g. sid.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Release
    ( Release(Release, releaseName, releaseAliases, releaseArchitectures, releaseComponents)
    , parseComponents
    , parseArchitectures
    , parseReleaseFile
    , getReleaseInfoRemote
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative((<*>)))
#endif
import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (IOException, try)
import Control.Lens (over, view)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import Data.Set as Set (Set, fromList)
import Data.Text (Text, unpack)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (readFile)
import Debian.Arch (Arch(..), parseArch)
import Debian.Codename (Codename, codename, parseCodename)
import qualified Debian.Control.Text as T (Control'(Control), fieldValue, Paragraph, Paragraph', parseControl)
import Extra.Except -- (HasLoc(withLoc), HasIOException(fromIOException), MonadIO, MonadIOError, withError)
import Debian.Repo.URI (dirFromURI, fileFromURI)
import Debian.Release (parseSection', Section(..))
import Debian.TH (here)
import Debian.URI (URI(uriPath), uriPathLens{-, uriToString'-})
import Debian.UTF8 as Deb (decode)
import Debian.VendorURI (VendorURI, vendorURI)
import Extra.Verbosity (qPutStr)
import Language.Haskell.TH.Syntax (Loc)
import Prelude hiding (readFile)
import System.FilePath ((</>))
--import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), prettyShow)
import Distribution.Pretty
import qualified Text.PrettyPrint.HughesPJClass as PP (text)
import "regex-compat-tdfa" Text.Regex (mkRegex, splitRegex)
--import Debug.Trace

-- |A file whose contents have been read into memory.
data File a = File { path :: Source, text :: Failing a } deriving Show

data Source = LocalPath FilePath | RemotePath URI

readFile :: FilePath -> IO (File T.Text)
readFile x = File <$> return (LocalPath x) <*> (try (T.readFile x) >>= return . either (\ (e :: IOException) -> Failure [show e]) Success)

instance Show Source where
    show (LocalPath p) = p
    show (RemotePath uri) = show uri

-- | A Debian Release is a named set of packages such as Jessie
-- (Debian version 7.0) or Precise (Ubuntu version 12.04.)  The
-- information here is obtained from the Release file in
-- dists/releasename.
data Release = Release { releaseName :: Codename
                       , releaseAliases :: [Codename]
                       , releaseArchitectures :: Set Arch
                       -- ^ e.g. amd64, i386, arm, etc, the set of architectures for this release - when a binary
                       -- package has architecture "all" or a source package has architecture "any" it means this.
                       , releaseComponents :: [Section] -- ^ Typically main, contrib, non-free
                       } deriving (Eq, Ord, Read, Show)

-- | This is used to construct the top directory.  Initially I had
-- "deb" and "deb-private", but now we are adding "deb8".
#if 0
class HasPoolDir a where
    poolDir :: a -> FilePath
#endif

instance Pretty Release where
    pretty x = pretty (releaseName x)

instance Pretty Section where
    pretty (Section s) = PP.text s

parseComponents :: Text -> [Section]
parseComponents compList =
    map parseSection' . splitRegex re . unpack  $ compList
    where
      re = mkRegex "[ ,]+"

parseReleaseFile :: FilePath -> Codename -> [Codename] -> IO Release
parseReleaseFile path' dist aliases =
    liftIO (readFile path') >>= return . parseRelease dist aliases

parseRelease :: Codename -> [Codename] -> File Text -> Release
parseRelease dist aliases file =
    case text file of
      Failure msgs -> error $ "Could not read " ++ show (path file) ++ ": " ++ show msgs
      Success t ->
          case T.parseControl (show (path file)) t of
            Left msg -> error $ "Failure parsing " ++ show (path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (File {path = path file, text = Success info}) dist aliases

-- | Turn a parsed Release file into a Release
makeReleaseInfo :: File T.Paragraph -> Codename -> [Codename] -> Release
makeReleaseInfo file@(File {text = Failure msgs}) _dist _aliases =
    error $ "Failure reading " ++ show (path file) ++ ": " ++ show msgs
makeReleaseInfo file@(File {text = Success info}) dist aliases =
    case (T.fieldValue "Architectures" info, T.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          Release { releaseName = dist
                  , releaseAliases = aliases
                  , releaseArchitectures = parseArchitectures archList
                  , releaseComponents = parseComponents compList }
      _ -> error $ "Missing Architectures or Components field in Release file " ++ show (path file)

parseArchitectures :: Text -> Set Arch
parseArchitectures archList =
    Set.fromList . map parseArch . splitRegex re . unpack $ archList
    where
      re = mkRegex "[ ,]+"

-- |Get the list of releases of a remote repository given the url for
-- one of its releases.
getReleaseInfoRemote :: [Loc] -> VendorURI -> IO [Release]
getReleaseInfoRemote locs uri = do
  --qPutStr ("getReleaseInfoRemote - uri=" <> show uri <> ") at " <> prettyShow ($here : locs))
  dir <- dirFromURI (distsURI uri)
  let codenames = fmap parseCodename $ filter (\x -> not (elem x [".", ".."])) dir
  --qPutStrLn ("getReleaseInfoRemote - dir=" <> show dir <> " at " <> prettyShow ($here : locs))
  mapM (verify locs uri) codenames
  -- qPutStrLn (prettyShow $here <> " - result=" ++ show result)
  --qPutStr (")\n code names: " <> show (fmap (codename . releaseName) result) <> "\n")

-- Verify by reading and parsing the Release file
verify :: [Loc] -> VendorURI -> Codename -> IO Release
verify locs uri cname = do
  --qPutStrLn (prettyShow $here <>  " - names=" ++ show (fmap codename names))
  (releaseFile :: File (T.Paragraph' Text)) <- getReleaseFile locs uri cname
  let (suite :: Codename) = getSuite releaseFile
  return $ makeReleaseInfo releaseFile cname [suite]
{-
  --qPutStrLn (prettyShow $here <>  " - #releaseFiles=" ++ show (Prelude.length releaseFiles))
  releaseTriples <- mapM (\(rfile, cname) -> (\x -> (x,rfile,cname)) <$> getSuite rfile) (zip releaseFile names)
  --let releasePairs = zip3 (map getSuite releaseFiles) (zip releaseFiles names)
  --qPutStrLn (prettyShow $here <>  " - #releaseTriples=" ++ show (Prelude.length releaseTriples))
  return $ map (uncurry3 getReleaseInfo) releaseTriples
-}

--getReleaseInfo :: Codename -> (File T.Paragraph) -> Release
--getReleaseInfo dist info = makeReleaseInfo info dist []

getSuite :: File (T.Paragraph' Text) -> Codename
getSuite (File {text = Success releaseFile}) =
    case T.fieldValue releaseNameField releaseFile of
      Nothing -> error "no release name field"
      Just x -> parseCodename (unpack x)
    where
      releaseNameField =
          case T.fieldValue "Suite" releaseFile of
            Just _ -> "Suite"
            Nothing -> case T.fieldValue "Codename" releaseFile of
                         Just _ -> "Codename"
                         Nothing -> error (prettyShow $here <> " - no releaeNameField in " ++ show releaseFile)
getSuite (File {text = Failure msgs}) = error $ intercalate "\n" msgs

getReleaseFile :: [Loc] -> VendorURI -> Codename -> IO (File (T.Paragraph' Text))
getReleaseFile locs uri dist =
          do qPutStr "."
             release <- fileFromURI Nothing relURI :: IO L.ByteString
             let control = (either (Left . userError . show) Right . T.parseControl (show relURI) . Deb.decode) release
             case control of
               Right (T.Control [info :: T.Paragraph' Text]) -> do
                 return $ File {path = RemotePath relURI, text = Success info}
               Left e -> error (intercalate "\n  "
                                  [ prettyShow ($here : locs) <> " - failed to get release info from dist "
                                  , "e=" ++ show (show e)
                                  , "codename=" <> show (codename dist)
                                  , "relURI=" <> show relURI
                                  , "distURI=" <> show distURI
                                  , "distsURI=" <> show (distsURI uri)
                                  ])
               Right (T.Control []) -> error (prettyShow $here ++ " - empty Release file")
               Right (T.Control _paras) -> error (prettyShow $here ++ " - multiple paragraphs in Release file")
          where
            relURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = over uriPathLens (</> (codename dist <> "/")) (distsURI uri)

--uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
--uncurry3 f (a, b, c) =  f a b c

distsURI :: VendorURI -> URI
distsURI = over uriPathLens (</> "dists/") . view vendorURI
