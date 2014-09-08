{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Verbosity
    ( modifyEnv
    , qPutStr
    , qPutStrLn
    , ePutStr
    , ePutStrLn
    , timeTask
    , quieter
    , noisier
    , withModifiedVerbosity
    , defaultVerbosity
    , readProc
    ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy as L (ByteString)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Posix.Env (setEnv, getEnv, unsetEnv)
import System.Process (CreateProcess)
import System.Process.ByteString.Lazy (Chunk, readProcessChunks, putIndentedShowCommand, putMappedChunks, insertCommandStart, eraseOutput)

import Control.Exception (evaluate)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

-- | Generalization of Posix setEnv/unSetEnv.
modifyEnv :: String -> (Maybe String -> Maybe String) -> IO ()
modifyEnv name f =
    getEnv name >>= maybe (unsetEnv name) (\ x -> setEnv name x True) . f

ePutStr :: MonadIO m => String -> m ()
ePutStr = liftIO . hPutStr stderr
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr

qPutStr :: MonadIO m => String -> m ()
qPutStr s = do
  v <- verbosity
  when (v > 0) (ePutStr s)

qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn s = do
  v <- verbosity
  when (v > 0) (ePutStrLn s)

-- | Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

quieter :: (MonadIO m, MonadMask m) => Int -> m a -> m a
quieter n action = withModifiedVerbosity (\ v -> v - n) action

noisier :: (MonadIO m, MonadMask m) => Int -> m a -> m a
noisier n action = withModifiedVerbosity (\ v -> v + n) action

withModifiedVerbosity :: (MonadIO m, MonadMask m) => (Int -> Int) -> m a -> m a
withModifiedVerbosity f action =
    bracket verbosity -- acquire
            (\ v0 -> liftIO (modifyEnv "VERBOSITY" (const (Just (show v0))))) -- release
            (\ v0 -> liftIO (modifyEnv "VERBOSITY" (const (Just (show (f v0))))) >> action)

defaultVerbosity :: Int
defaultVerbosity = 0

verbosity :: MonadIO m => m Int
verbosity = liftIO $ getEnv "VERBOSITY" >>= return . maybe 1 read

readProc :: MonadIO m => CreateProcess -> L.ByteString -> m [Chunk L.ByteString]
readProc p input = do
  v <- verbosity
  case v of
    n | n <= 0 -> liftIO $ readProcessChunks p input
    1 -> liftIO $ readProcessChunks p input >>= putMappedChunks (insertCommandStart p . eraseOutput)
    _ -> liftIO $ readProcessChunks p input >>= putIndentedShowCommand p " 1> " " 1> "
