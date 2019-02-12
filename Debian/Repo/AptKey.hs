{-# LANGUAGE ConstraintKinds, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Debian.Repo.AptKey
    ( AptKey(AptKey)
    , HasAptKey(aptKey)
    , MonadApt
    ) where

import Control.Lens (_2, Getter)
import Control.Monad.Reader (MonadReader)
import Debian.Repo.EnvPath (EnvRoot)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

class HasAptKey r where aptKey :: Getter r AptKey
instance HasAptKey AptKey where aptKey = id

instance HasAptKey (a, AptKey) where aptKey = _2
instance HasAptKey (a, AptKey, b) where aptKey = _2

type MonadApt r m = (HasAptKey r, MonadReader r m)
