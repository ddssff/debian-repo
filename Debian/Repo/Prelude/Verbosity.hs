{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Verbosity
    ( modifyEnv
    , qPutStr
    , qPutStrLn
    , ePutStr
    , ePutStrLn
    , quieter
    , noisier
    , withModifiedVerbosity
    , verbosity
    ) where

import Control.Monad (when)
import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.Trans (liftIO, MonadIO)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Posix.Env (getEnv, setEnv, unsetEnv)

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

quieter :: (MonadIO m, MonadMask m) => Int -> m a -> m a
quieter n action = withModifiedVerbosity (\ v -> v - n) action

noisier :: (MonadIO m, MonadMask m) => Int -> m a -> m a
noisier n action = withModifiedVerbosity (\ v -> v + n) action

withModifiedVerbosity :: (MonadIO m, MonadMask m) => (Int -> Int) -> m a -> m a
withModifiedVerbosity f action =
    bracket verbosity -- acquire
            (\ v0 -> liftIO (modifyEnv "VERBOSITY" (const (Just (show v0))))) -- release
            (\ v0 -> liftIO (modifyEnv "VERBOSITY" (const (Just (show (f v0))))) >> action)

verbosity :: MonadIO m => m Int
#ifdef NO_VERBOSITY_CONTROL
verbosity = return 1
#else
verbosity = liftIO $ getEnv "VERBOSITY" >>= return . maybe 1 read
#endif
