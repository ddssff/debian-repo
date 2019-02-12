module Debian.Repo.DebError
    ( DebError(..)
    ) where

import Control.Exception (Exception, IOException)
import Debian.Except (HasIOException(fromIOException))
import Debian.Repo.Rsync (HasRsyncError(fromRsyncError), RsyncError)
import Debian.URI (HasParseError(fromParseError), HasURIError(fromURIError), URIError)
import Language.Haskell.TH.Syntax (Loc)
import Text.Parsec (ParseError)

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
