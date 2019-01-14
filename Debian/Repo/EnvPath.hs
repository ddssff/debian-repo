{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.EnvPath
    ( EnvRoot(..), rootPath
    , EnvPath(..), envRoot, envPath
    , outsidePath
    , appendPath
    , rootEnvPath
    ) where

import Control.Lens (makeLenses, over)
import Debian.Pretty (PP(PP))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- |The root directory of an OS image.
data EnvRoot = EnvRoot { _rootPath :: FilePath } deriving (Ord, Eq, Read, Show)

-- |A directory inside of an OS image.
data EnvPath = EnvPath { _envRoot :: EnvRoot
                       , _envPath :: FilePath
                       } deriving (Ord, Eq, Read, Show)

$(makeLenses ''EnvRoot)
$(makeLenses ''EnvPath)

outsidePath :: EnvPath -> FilePath
outsidePath path = _rootPath (_envRoot path) ++ _envPath path

appendPath :: FilePath -> EnvPath -> EnvPath
appendPath suff path = over envPath (++ suff) path

rootEnvPath :: FilePath -> EnvPath
rootEnvPath s = EnvPath { _envRoot = EnvRoot "", _envPath = s }

instance Pretty (PP EnvRoot) where
    pPrint (PP x) = text (_rootPath x)
