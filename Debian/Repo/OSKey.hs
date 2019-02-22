{-# LANGUAGE ConstraintKinds, FlexibleInstances #-}

module Debian.Repo.OSKey
    ( OSKey(..)
    , HasOSKey(..)
    )  where

import Control.Lens (_2, _3, Getter)
--import Control.Monad.Reader (MonadReader)
import Extra.EnvPath (EnvRoot)

newtype OSKey = OSKey {_root :: EnvRoot} deriving (Eq, Ord, Show)

class HasOSKey r where osKey :: Getter r OSKey
instance HasOSKey OSKey where osKey = id

instance HasOSKey (a, OSKey) where osKey = _2
instance HasOSKey (a, b, OSKey) where osKey = _3
