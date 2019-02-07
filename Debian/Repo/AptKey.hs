{-# LANGUAGE ConstraintKinds, FlexibleInstances #-}

module Debian.Repo.AptKey
    ( AptKey(AptKey)
    , HasAptKey(aptKey)
    , MonadApt
    , runV, runVE, runQ, runQE
    ) where

import Control.Lens (_2, Getter, view)
import Control.Monad.Catch (MonadCatch, SomeException, try)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.String (IsString)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.Prelude.Process (run, run2, RunOptions(..), showCommandAndResult, putIndented)
import Debian.Repo.Prelude.Verbosity (ePutStrLn)
import Language.Haskell.TH.Syntax (Loc)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess)
import System.Process.ListLike (ListLikeProcessIO, showCreateProcessForUser)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

class HasAptKey r where aptKey :: Getter r AptKey
instance HasAptKey AptKey where aptKey = id

instance HasAptKey (a, AptKey) where aptKey = _2
instance HasAptKey (a, AptKey, b) where aptKey = _2

type MonadApt r m = (HasAptKey r, MonadReader r m)

runV ::
    (Eq c, IsString a, ListLikeProcessIO a c, MonadApt r m, MonadError e m, MonadIO m, MonadCatch m)
    => Loc -> CreateProcess -> a -> m (ExitCode, a, a)
runV loc p input =
    run2 loc (StartMessage showCommand' <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

runVE ::
    (Eq c, IsString a, ListLikeProcessIO a c, MonadApt r m, MonadError e m, MonadIO m, MonadCatch m)
    => Loc -> CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runVE loc p input = try $ runV loc p input

runQ ::
    (ListLikeProcessIO a c, MonadApt r m, MonadError e m, MonadIO m, MonadCatch m)
    => Loc -> CreateProcess -> a -> m (ExitCode, a, a)
runQ loc p input =
    run2 loc (StartMessage showCommand' <> FinishMessage showCommandAndResult) p input

runQE ::
    (ListLikeProcessIO a c, MonadApt r m, MonadError e m, MonadIO m, MonadCatch m)
    => Loc -> CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
runQE loc p input = try $ runQ loc p input

showCommand' :: (MonadApt r m, MonadIO m) => String -> CreateProcess -> m ()
showCommand' prefix p = view aptKey >>= \key -> ePutStrLn (prefix ++ showCreateProcessForUser p ++ " (in " ++ show key ++ ")")
