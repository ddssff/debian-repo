{-# LANGUAGE ConstraintKinds, FlexibleInstances, RankNTypes #-}
{-# OPTIONS -Wall #-}

module Debian.Repo.AptKey
    ( AptKey
    , aptKey
    , MonadApt
    ) where

import Control.Lens (_2, Lens', view)
import Control.Monad.Reader (MonadReader)
import Extra.EnvPath (EnvRoot, HasEnvRoot(envRootLens))

type AptKey = EnvRoot

aptKey :: forall r. HasEnvRoot r => Lens' r AptKey
aptKey = envRootLens

instance HasEnvRoot (a, EnvRoot) where envRootLens = _2
instance HasEnvRoot (a, EnvRoot, b) where envRootLens = _2

type MonadApt r m = (HasEnvRoot r, MonadReader r m)
