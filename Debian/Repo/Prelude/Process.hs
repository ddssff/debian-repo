{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Process
    ( timeTask
    , readProcLazy
    , readProcLazy'
    , throwProcessResult'
    , throwProcessResult''
    , throwProcessFailure
    , mapResultM
    , testExit
    , processException
    , readProcFailing
    , insertProcessEnv
    , modifyProcessEnv
    ) where

import Control.Arrow (second)
import Control.Exception (evaluate, Exception, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy as L (ByteString)
import Data.String (IsString)
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)
import Debian.Repo.Prelude.Verbosity (verbosity)
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(cmdspec, cwd, env))
import System.Process.Chunks (Chunk(..), insertCommandStart, putIndentedShowCommand, putMappedChunks, readCreateProcessChunks, showCmdSpecForUser)
import System.Process.ListLike (ListLikeLazyIO)

-- | Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

-- | Verbosity enabled process reader.  (Why MonadIO and not IO?)
readProcLazy :: MonadIO m => CreateProcess -> L.ByteString -> m [Chunk L.ByteString]
readProcLazy p input = do
  v <- verbosity
  case v of
    n | n <= 0 -> liftIO $ readCreateProcessChunks p input
    1 -> liftIO $ readCreateProcessChunks p input >>= putMappedChunks (insertCommandStart p . filter (not . isOutput))
    _ -> liftIO $ readCreateProcessChunks p input >>= putIndentedShowCommand p " 1> " " 1> "

isOutput :: Chunk a -> Bool
isOutput (Stdout _) = True
isOutput (Stderr _) = True
isOutput _ = False

-- | Verbosity enabled process reader.  (Why MonadIO and not IO?)
readProcLazy' :: (ListLikeLazyIO a c, IsString a, Eq c, MonadIO m) => CreateProcess -> a -> m [Chunk a]
readProcLazy' p input = liftIO $ do
  v <- verbosity
  case v of
    n | n <= 0 -> readCreateProcessChunks p input
    1 -> readCreateProcessChunks p input >>= putMappedChunks (insertCommandStart p . filter (not . isOutput))
    _ -> readCreateProcessChunks p input >>= putIndentedShowCommand p " 1> " " 1> "

throwProcessResult' :: (ExitCode -> Maybe IOError) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult' f p chunks = mapResultM (\ code -> maybe (return $ Result code) (throw $ processException p code) (f code)) chunks

throwProcessResult'' :: Exception e => (CreateProcess -> ExitCode -> Maybe e) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult'' f p chunks = mapResultM (\ code -> maybe (return $ Result code) throw (f p code)) chunks

throwProcessFailure :: CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessFailure p = throwProcessResult' (testExit Nothing (Just . processException p . ExitFailure)) p

mapResultM :: Monad m => (ExitCode -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
mapResultM f chunks = mapM (\ x -> case x of Result code -> f code; _ -> return x) chunks

testExit :: a -> (Int -> a) -> ExitCode -> a
testExit s _ ExitSuccess = s
testExit _ f (ExitFailure n) = f n

-- | Copied from "System.Process", the exception thrown when the
-- process started by 'System.Process.readProcess' gets an
-- 'ExitFailure'.
processException :: CreateProcess -> ExitCode -> IOError
processException p code =
    mkIOError OtherError (showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> "(in " ++ show d ++ ")") (cwd p) ++ " -> " ++ show code) Nothing Nothing

-- | Verbosity enabled process reader that throws an exception on ExitFailure.
readProcFailing :: MonadIO m => CreateProcess -> L.ByteString -> m [Chunk L.ByteString]
readProcFailing p input = readProcLazy p input >>= liftIO . throwProcessFailure p

-- | Set an environment variable in the CreateProcess, initializing it
-- with what is in the current environment.
insertProcessEnv :: [(String, String)] -> CreateProcess -> IO CreateProcess
insertProcessEnv pairs = modifyProcessEnv (map (second Just) pairs)
{-
insertEnv pairs p = do
    pairs' <- maybe (getEnvironment >>= return . (++ pairs)) return (env p)
    return p {env = Just pairs'}

modEnv :: [(String, Maybe String)] -> [(String, String)] -> [(String, String)]
modEnv [] env0 = env0
modEnv pairs env0 = foldl modEnv1 env0 pairs
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-}

modEnv1 :: [(String, String)] -> (String, Maybe String) -> [(String, String)]
modEnv1 env0 (name, mvalue) = maybe [] (\ v -> [(name, v)]) mvalue ++ filter ((/= name) . fst) env0

modifyProcessEnv :: [(String, Maybe String)] -> CreateProcess -> IO CreateProcess
modifyProcessEnv pairs p = do
  env0 <- maybe getEnvironment return (env p)
  let env' = foldl modEnv1 env0 pairs
  return $ p {env = Just env'}
