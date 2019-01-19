{-# LANGUAGE ConstraintKinds, FlexibleInstances #-}

module Debian.Repo.AptKey
    ( AptKey(AptKey)
    , HasAptKey(aptKey)
    , MonadApt
    , runV, runVE, runQ, runQE
    ) where

import Control.Monad.Catch (MonadCatch, SomeException, try)
import Control.Lens (_2, Getter, view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.String (IsString)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.Prelude.Process (run, RunOptions(..), showCommandAndResult, putIndented)
import Debian.Repo.Prelude.Verbosity (ePutStrLn)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess)
import System.Process.ListLike (ListLikeProcessIO, showCreateProcessForUser)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

class HasAptKey r where aptKey :: Getter r AptKey
instance HasAptKey AptKey where aptKey = id

instance HasAptKey (a, AptKey) where aptKey = _2
instance HasAptKey (a, AptKey, b) where aptKey = _2

type MonadApt r m = (HasAptKey r, MonadReader r m)

runV :: (Eq c, IsString a, ListLikeProcessIO a c, MonadApt r m, MonadIO m) => CreateProcess -> a -> m (ExitCode, a, a)
runV p input =
    run (StartMessage showCommand' <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

runVE :: (Eq c, IsString a, ListLikeProcessIO a c, MonadApt r m, MonadIO m, MonadCatch m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runVE p input = try $ runV p input

runQ :: (ListLikeProcessIO a c, MonadApt r m, MonadIO m) => CreateProcess -> a -> m (ExitCode, a, a)
runQ p input =
    run (StartMessage showCommand' <> FinishMessage showCommandAndResult) p input

runQE :: (ListLikeProcessIO a c, MonadApt r m, MonadIO m, MonadCatch m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runQE p input = try $ runQ p input

showCommand' :: (MonadApt r m, MonadIO m) => String -> CreateProcess -> m ()
showCommand' prefix p = view aptKey >>= \key -> ePutStrLn (prefix ++ showCreateProcessForUser p ++ " (in " ++ show key ++ ")")
