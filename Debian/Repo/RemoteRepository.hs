{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.RemoteRepository
    ( RemoteRepository(..)
    ) where

import qualified Debian.Pretty as F (Pretty(..))
import Debian.Repo.Release (Release)
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey(Remote))
import Debian.URI (fromURI', URI')

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show, Eq, Ord)

instance Repo RemoteRepository where
    repoKey (RemoteRepository uri _) = Remote uri
    repoReleaseInfo (RemoteRepository _ info) = info

-- | URI has a bogus show function, which we are using here.
instance F.Pretty URI' where
    pretty = F.pretty . show . fromURI'

instance F.Pretty RemoteRepository where
    pretty (RemoteRepository s _) = F.pretty s
