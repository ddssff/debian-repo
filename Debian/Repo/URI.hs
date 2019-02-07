{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, StandaloneDeriving, TemplateHaskell #-}

module Debian.Repo.URI
    ( fileFromURI
    , fileFromURIStrict
    , dirFromURI
    ) where

--import Control.Exception (catch, IOException, throw)
import Control.Lens (ix, makePrisms, makeLensesFor, preview)
import Control.Monad.Except (MonadError, MonadPlus, throwError)
import Control.Monad.Trans (MonadIO)
import Data.ByteString.Lazy.UTF8 as L hiding (fromString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (msum)
import Data.Generics (Data, Typeable, listify)
import Data.Maybe (mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text as T (init, last, pack, Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy as LT (pack, Text)
import Debian.Except (HasIOException(fromIOException), liftEIO)
import Debian.Repo.EnvPath (EnvRoot, _rootPath)
import Debian.Repo.Prelude.Process (run')
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.TH (here)
import Distribution.Pretty (prettyShow)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Loc)
import Network.URI (URI(..), URIAuth(..), uriToString {-nullURI, parseURIReference, parseURI, parseAbsoluteURI, parseRelativeReference-})
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess, proc)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import System.Process.Common ({-ListLikeProcessIO,-} showCreateProcessForUser)
--import Test.QuickCheck (Arbitrary)
--import Text.Parsec (ParseError)
--import Text.Regex (mkRegex, matchRegex)
import Text.Taggy (parseDOM, Node(..), Element(..))

deriving instance Data Node
deriving instance Data Element

-- data Node = NodeElement Element | NodeContent Text
$(makePrisms ''Node)
$(makeLensesFor [("eltName", "eltNameLens"),
                 ("eltAttrs", "eltAttrsLens"),
                 ("eltChildren", "eltChildrenLens")] ''Element)

fileFromURI :: (MonadIO m, HasIOException e, MonadError e m) => Loc -> Maybe EnvRoot -> URI -> m L.ByteString
fileFromURI loc chroot uri = fileFromURIStrict loc chroot uri

fileFromURIStrict :: (MonadIO m, HasIOException e, MonadError e m) => Loc -> Maybe EnvRoot -> URI -> m L.ByteString
fileFromURIStrict loc chroot uri = do
    let chroot' = maybe "" _rootPath chroot
    qPutStrLn (prettyShow $here <> " - fileFromURIStrict chroot=" <> show chroot <> " uri=" ++ show uri)
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> liftEIO loc $ L.readFile (chroot' </> uriPath uri)
      -- This happens - not sure why
      ("file:", Just (URIAuth "" "" "")) -> liftEIO loc $ L.readFile (uriPath uri)
      -- ("ssh:", Just auth) -> cmdOutputStrict ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) ->
          run' loc (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri])
      _ ->
          run' loc (proc "curl" ["-s", "-g", "-l", uriToString' uri])

textFromHttpURI :: T.Text -> IO LT.Text
textFromHttpURI uri = (LT.pack . L.toString) <$> run' $here (proc "curl" ["-s", "-g", T.unpack uri])

domFromHttpURI :: T.Text -> IO [Node]
domFromHttpURI uri = parseDOM True <$> textFromHttpURI uri

filesFromHttpURI :: T.Text -> IO (Maybe [String])
filesFromHttpURI uri = domFromHttpURI uri >>= files1
    where
      files1 :: [Node] -> IO (Maybe [FilePath])
      files1 dom =
          case preview (ix 0 . _NodeElement . eltChildrenLens . ix 0 . _NodeElement . eltChildrenLens) dom of
            Just [NodeElement
                  (Element {eltName = "head",
                            eltChildren = [NodeElement
                                           (Element {eltName = "title",
                                                     eltChildren = [NodeContent "301 Moved Permanently"]})]}),
                  NodeElement (Element {eltName = "body", eltChildren = body})] -> moved body
            _ -> return $ files2 dom
      moved :: [Node] -> IO (Maybe [FilePath])
      moved body =
          case (preview (ix 0 . _NodeElement . eltChildrenLens . ix 0 . _NodeContent) body,
                preview (ix 1 . _NodeElement . eltChildrenLens . ix 1 . _NodeElement . eltAttrsLens . ix "href") body) of
            (Just "Moved Permanently", Just uri') -> filesFromHttpURI uri'
      files2 :: [Node] -> Maybe [FilePath]
      files2 dom = Just (mapMaybe testNode (gFind dom) :: [FilePath])
      testNode :: Node -> Maybe FilePath
      testNode node
          | preview (_NodeElement . eltNameLens) node == Just (T.pack "a") =
              -- The entries have an "a" link, and the ones we are interested
              -- in seem to have matching content and href.
              case preview (_NodeElement . eltAttrsLens . ix "href") node of
                Nothing -> Nothing
                Just path1 ->
                    let path2 = preview (_NodeElement . eltChildrenLens . ix 0 . _NodeContent) node in
                    if T.last path1 == '/' && Just path1 == path2 then Just (T.unpack (T.init path1)) else Nothing
      testNode node = Nothing

dirFromURI :: Loc -> URI -> IO [String]
dirFromURI loc uri = do
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> getDirectoryContents (uriPath uri)
      ("file:", Just (URIAuth "" "" "")) -> getDirectoryContents (uriPath uri)
      ("ssh:", Just auth) ->
          (Prelude.lines . L.toString) <$>
            run' loc (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "ls", "-1", uriPath uri])
      ("http:", _) ->
          maybe (error ("Could not get files from " ++ show uri)) id <$> filesFromHttpURI (T.pack (uriToString' uri))
      (scheme, _) -> error (prettyShow $here <> " - unexpected URI scheme: " <> show scheme)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

run :: ExpQ
run = [|runCreateProcess $here|]

runCreateProcess ::
    (MonadIO m, HasIOException e, MonadError e m)
    => Loc
    -> CreateProcess
    -> m L.ByteString
runCreateProcess loc cp = do
  (code, out, err) <- liftEIO loc $ readCreateProcessWithExitCode cp L.empty
  case code of
    ExitSuccess -> return out
    ExitFailure _ -> throwError $ fromIOException loc $ userError $ unlines $
                                       [ show code
                                       , " command: " ++ showCreateProcessForUser cp
                                       , " stderr: " ++ unpack (decodeUtf8 (L.toStrict err))
                                       , " stdout: " ++ unpack (decodeUtf8 (L.toStrict out))
                                       , " location: " ++ show loc ]

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
