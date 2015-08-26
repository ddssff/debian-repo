{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Process
    ( timeTask
    , readProcessVE
    , readProcessV
    , readProcessQE
    -- , throwProcessResult'
    -- , throwProcessResult''
    -- , throwProcessFailure
    -- , mapResultM
    , testExit
    , processException
    , insertProcessEnv
    , modifyProcessEnv
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Arrow (second)
import Control.Exception (evaluate, SomeException, try)
import Control.Monad.State (evalState, StateT, get, put)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ListLike (break, head, hPutStr, null, singleton, tail)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif
import Data.String (IsString(fromString))
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)
import Debian.Repo.Prelude.Verbosity (ePutStrLn)
import GHC.IO.Exception (IOErrorType(OtherError))
import Prelude hiding (break, head, null, tail)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (stdout, stderr)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(cwd, env))
import System.Process.ListLike (Chunk(..), collectOutput, ListLikeProcessIO, ProcessOutput, readCreateProcessLazy, showCreateProcessForUser)

-- | Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

readProcessVE :: (Eq c, IsString a, ListLikeProcessIO a c, MonadIO m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
readProcessVE p input = do
    -- liftIO $ try $ readProcessV p input
  ePutStrLn (" -> " ++ showCreateProcessForUser p)
  result <- liftIO $ try $ readCreateProcessLazy p input >>= putIndented >>= return . collectOutput
  ePutStrLn (" <- " ++ showCreateProcessForUser p ++ " -> " ++  either show (\ (code, _, _) -> show code) result)
  return result

readProcessV :: forall a c m. (Eq c, IsString a, ProcessOutput a (ExitCode, a, a), ListLikeProcessIO a c, MonadIO m) => CreateProcess -> a -> m (ExitCode, a, a)
readProcessV p input = do
  ePutStrLn (" -> " ++ showCreateProcessForUser p)
  result@(code, _, _) <- liftIO $ readCreateProcessLazy p input >>= putIndented >>= return . collectOutput
  ePutStrLn (" <- " ++ showCreateProcessForUser p ++ " -> " ++ show code)
  return result

readProcessQE :: (Eq c, IsString a, ListLikeProcessIO a c, MonadIO m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
readProcessQE p input = do
    -- liftIO $ try $ readProcessV p input
  ePutStrLn (" -> " ++ showCreateProcessForUser p)
  result <- liftIO $ try $ readCreateProcessLazy p input >>= return . collectOutput
  ePutStrLn (" <- " ++ showCreateProcessForUser p ++ " -> " ++  either show (\ (code, _, _) -> show code) result)
  return result

putIndented :: forall a c. (Eq c, ListLikeProcessIO a c, IsString a) => [Chunk a] -> IO [Chunk a]
putIndented chunks =
    mapM_ echo (indentChunks "     1> " "     2> " chunks) >> return chunks
    where
      echo :: Chunk a -> IO (Chunk a)
      echo c@(Stdout x) = hPutStr stdout x >> return c
      echo c@(Stderr x) = hPutStr stderr x >> return c
      echo c = return c

-- | Pure function to indent the text of a chunk list.
indentChunks :: forall a c. (ListLikeProcessIO a c, Eq c, IsString a) => String -> String -> [Chunk a] -> [Chunk a]
indentChunks outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl (fromString outp) (fromString errp)) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

-- | The monad state, are we at the beginning of a line or the middle?
data BOL = BOL | MOL deriving (Eq)

-- | Indent the text of a chunk with the prefixes given for stdout and
-- stderr.  The state monad keeps track of whether we are at the
-- beginning of a line - when we are and more text comes we insert one
-- of the prefixes.
indentChunk :: forall a c m. (Monad m, Functor m, ListLikeProcessIO a c, Eq c) => c -> a -> a -> Chunk a -> StateT BOL m [Chunk a]
indentChunk nl outp errp chunk =
    case chunk of
      Stdout x -> doText Stdout outp x
      Stderr x -> doText Stderr errp x
      _ -> return [chunk]
    where
      doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = break (== nl) x
        (<>) <$> doHead con pre hd <*> doTail con pre tl
      doHead :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doHead _ _ x | null x = return []
      doHead con pre x = do
        bol <- get
        case bol of
          BOL -> put MOL >> return [con (pre <> x)]
          MOL -> return [con x]
      doTail :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doTail _ _ x | null x = return []
      doTail con pre x = do
        bol <- get
        put BOL
        tl <- doText con pre (tail x)
        return $ (if bol == BOL then [con pre] else []) <> [con (singleton nl)] <> tl

{-
readProcessV p input = liftIO $ do
  v <- verbosity
  case v of
    n | n <= 0 -> readCreateProcessWithExitCode p input >>= putMapped (insertResult . insertStart p . filter (not . isOutput))
    1 -> readCreateProcess p input >>= putMapped (insertResult . insertStart p . dotifyChunks 100)
    _ -> readCreateProcess p input >>= putIndentedShowCommand p " 1> " " 2> "
    where
      putMapped :: (OutputChunks a -> OutputChunks a) -> OutputChunks a -> IO (OutputChunks a)
      putMapped f ocs = putMappedChunks (\ xs -> unOutputChunks $ f $ OutputChunks xs) (unOutputChunks ocs)
      isOutput (Stdout _) = True
      isOutput (Stderr _) = True
      isOutput _ = False
-}

-- readProcFailing :: (ListLikeLazyIO a c, IsString a, Eq c, MonadIO m) => CreateProcess -> a -> m [Chunk a]
-- readProcFailing p input = readProcLazy p input >>= liftIO . throwProcessFailure p

-- | Turn process exit codes into exceptions.
{-
throwProcessResult' :: (ExitCode -> Maybe IOError) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult' f p chunks = mapResultM (\ code -> maybe (return $ Result code) (throw $ processException p code) (f code)) chunks

-- | Turn process exit codes into exceptions with access to the
-- original CreateProcess record.
throwProcessResult'' :: Exception e => (CreateProcess -> ExitCode -> Maybe e) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult'' f p chunks = mapResultM (\ code -> maybe (return $ Result code) throw (f p code)) chunks

throwProcessFailure :: CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessFailure p = throwProcessResult' (testExit Nothing (Just . processException p . ExitFailure)) p

mapResultM :: Monad m => (ExitCode -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
mapResultM f chunks = mapM (\ x -> case x of Result code -> f code; _ -> return x) chunks
-}

testExit :: a -> (Int -> a) -> ExitCode -> a
testExit s _ ExitSuccess = s
testExit _ f (ExitFailure n) = f n

-- | Copied from "System.Process", the exception thrown when the
-- process started by 'System.Process.readProcess' gets an
-- 'ExitFailure'.
processException :: CreateProcess -> ExitCode -> IOError
processException p code =
    mkIOError OtherError (showCreateProcessForUser p ++ maybe "" (\ d -> "(in " ++ show d ++ ")") (cwd p) ++ " -> " ++ show code) Nothing Nothing

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
