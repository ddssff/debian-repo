{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Verbosity
    ( qPutStr
    , qPutStrLn
    , ePutStr
    , ePutStrLn
    , timeTask
    , quieter
    , noisier
    , withModifiedVerbosity
    , defaultVerbosity
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStr, hPutStrLn, stderr)

import Control.Exception (evaluate)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

ePutStr :: MonadIO m => String -> m ()
ePutStr = liftIO . hPutStr stderr
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr

qPutStr :: MonadIO m => String -> m ()
qPutStr = ePutStr
qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn = ePutStrLn

-- | Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

quieter :: Int -> a -> a
quieter _ = id

noisier :: Int -> a -> a
noisier _ = id

withModifiedVerbosity :: b -> a -> a
withModifiedVerbosity _ x = x

defaultVerbosity :: Int
defaultVerbosity = 0
