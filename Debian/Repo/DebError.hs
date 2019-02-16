module Debian.Repo.DebError
    ( DebError(..)
    ) where

import Control.Exception (Exception, displayException, IOException)
import Extra.Except (HasIOException(fromIOException))
import Debian.Repo.Rsync (HasRsyncError(fromRsyncError), RsyncError)
import Debian.URI (HasParseError(fromParseError), HasURIError(fromURIError), URIError)
import Distribution.Pretty (Pretty(pretty), prettyShow)
import Extra.Except (HasLoc(withLoc))
import Language.Haskell.TH.Syntax (Loc)
import Text.Parsec (ParseError)
import Text.PrettyPrint (text)

data DebError
    = IOException IOException
    | URIError URIError
    | ParseError ParseError
    | RsyncError RsyncError
    | DebErrorLoc Loc DebError
    deriving (Show, Eq, Ord)

instance Exception DebError

instance HasIOException DebError where fromIOException = IOException
instance HasParseError DebError where fromParseError = ParseError
instance HasURIError DebError where fromURIError = URIError
instance HasRsyncError DebError where fromRsyncError = RsyncError
instance HasLoc DebError where withLoc = DebErrorLoc

instance Pretty DebError where
    pretty (IOException e) = text $ "(" ++ displayException e ++ " :: IOException)"
    pretty (URIError e) = text $ "(URIError " <> show e <> " :: DebError)"
    pretty (ParseError e) = text $ "(ParseError " <> show e <> " :: DebError)"
    pretty (RsyncError e) = text $ "(RsyncError " <> show e <> " :: DebError)"
    pretty (DebErrorLoc loc e) = text $ "(DebErrorLoc (" <> prettyShow loc <> ") (" <> show e <> ") :: DebError)"
