import Control.DeepSeq (force)
import Data.ByteString.Char8 as B (ByteString, readFile)
import Data.Char (chr)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.IO as T
import Data.Word (Word8)
import Debian.Control
import Debian.Control.Text as T (parseControl, decodeControl)
import Debian.Pretty (pretty, render)
import System.Environment (getArgs)
import Text.Parsec.Error

main =
    do args <- getArgs
       case args of
             -- ("string" : _) -> (parseControlFromFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" :: IO (Either ParseError (Control' String))) >>= either (error . show) useControl
             -- ("byte" : _) ->   (parseControlFromFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" :: IO (Either ParseError (Control' B.ByteString))) >>= either (error . show) useControl
             -- ("byte2" : _) ->  (B.readFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" >>= return . B.parseControl "input" :: IO (Either ParseError (Control' B.ByteString))) >>= either (error . show) useControl
             -- bad
             ("text" : _) ->   (T.readFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" >>= return . T.parseControl "input" :: IO (Either ParseError (Control' Text))) >>= either (error . show) (return . useControl)
             -- better
             ("text2" : _) ->  (B.readFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" >>= return . T.parseControl "input" . decode :: IO (Either ParseError (Control' Text))) >>= either (error . show) (return . useControl)
             ("text3" : _) ->  (B.readFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" >>= return . T.parseControl "input" . decodeUtf8 :: IO (Either ParseError (Control' Text))) >>= either (error . show) (return . useControl)
             -- best - similar to byte2
             ("text4" : _) ->  (parseControlFromFile "/srv/deb/ubuntu/dists/precise-seereason/main/binary-i386/Packages" :: IO (Either ParseError (Control' B.ByteString))) >>= either (error . show) (return . useControl . T.decodeControl)

useControl = force . T.length . render . pretty


decode :: B.ByteString -> T.Text
decode b = decodeUtf8With e b
    where
      e :: String -> Maybe Word8 -> Maybe Char
      e _description w = fmap (chr . fromIntegral) w
