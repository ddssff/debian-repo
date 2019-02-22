{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.RemoteRepository
    ( RemoteRepository(..)
    ) where

--import Data.Map (Map)
--import Debian.Codename (Codename)
import Debian.Pretty (PP(..), ppPrint)
import Debian.Repo.Release (Release)
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey(Remote))
import Debian.URI (fromURI', URI')
import Text.PrettyPrint.HughesPJClass (text)
import Distribution.Pretty (Pretty(pretty))

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show, Eq, Ord)

instance Repo RemoteRepository where
    repoKey (RemoteRepository uri _) = Remote uri
    repoReleaseInfo (RemoteRepository _ info) = info

-- | URI has a bogus show function, which we are using here.
instance Pretty (PP URI') where
    pretty = text . show . fromURI' . unPP

instance Pretty (PP RemoteRepository) where
    pretty (PP (RemoteRepository s _)) = ppPrint s
