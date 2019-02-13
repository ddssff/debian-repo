module Debian.Repo.DebError
    ( DebError(..)
    ) where

import Control.Exception (Exception, displayException, IOException)
import Debian.Except (HasIOException(fromIOException))
import Debian.Repo.Rsync (HasRsyncError(fromRsyncError), RsyncError)
import Debian.URI (HasParseError(fromParseError), HasURIError(fromURIError), URIError)
import Distribution.Pretty (Pretty(pretty))
import Language.Haskell.TH.Syntax (Loc)
import Text.Parsec (ParseError)
import Text.PrettyPrint (text)

data DebError
    = IOException [Loc] IOException
    | URIError URIError
    | ParseError ParseError
    | RsyncError RsyncError
    deriving (Show, Eq, Ord)

instance Exception DebError

instance HasIOException DebError where fromIOException = IOException
instance HasParseError DebError where fromParseError = ParseError
instance HasURIError DebError where fromURIError = URIError
instance HasRsyncError DebError where fromRsyncError = RsyncError

instance Pretty DebError where
    pretty (IOException locs e) = text ("(" ++ displayException e ++ " :: IOException) at ") <> pretty locs
    pretty (URIError e) = text ("(URIError " <> show e <> " :: DebError)")
    pretty (ParseError e) = text ("(ParseError " <> show e <> " :: DebError)")
    pretty (RsyncError e) = text ("(RsyncError " <> show e <> " :: DebError)")
