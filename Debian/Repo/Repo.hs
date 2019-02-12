{-# LANGUAGE FlexibleContexts, FlexibleInstances, PackageImports, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Repo
    ( Repo(..)
    , RepoKey(..)
    , repoURI
    , repoKeyURI
    , repoArchList
    , libraryCompatibilityLevel
    , compatibilityFile
    ) where

import Control.Exception (IOException)
import Control.Lens (review, view)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Set (Set, unions)
import Data.Text (unpack)
import Debian.Arch (Arch)
import Debian.Except (MonadIO, MonadError)
import Debian.Repo.EnvPath (EnvPath, envPath)
import Debian.Repo.Release (Release(releaseArchitectures))
import Debian.Repo.URI (fileFromURI)
import Debian.TH (here)
import Debian.URI (fromURI', URI')
import qualified Debian.UTF8 as Deb (decode)
import Debian.VendorURI (VendorURI, vendorURI)
import Network.URI (parseURI, URI(uriPath))
import System.FilePath ((</>))

data RepoKey
    = Remote URI'
    | Local EnvPath
      deriving (Read, Show, Eq, Ord)

class (Ord t, Eq t) => Repo t where
    repoKey :: t -> RepoKey
    repositoryCompatibilityLevel :: (MonadIO m, MonadError IOException m) => t -> m (Maybe Int)
    repositoryCompatibilityLevel r =
        (parse . unpack . Deb.decode) <$> fileFromURI [$here] Nothing uri'
        where
          uri' = uri {uriPath = uriPath uri </> compatibilityFile}
          uri = case repoKey r of
                  Remote x -> fromURI' x
                  Local x -> fromJust . parseURI $ "file://" ++ view envPath x
          parse :: String -> Maybe Int
          parse s = case takeWhile isDigit s of
                         "" -> Nothing
                         s' -> Just . read $ s'
    -- | This method returns a list of all the release in the
    -- repository.  This can be used to identify all of the files
    -- in the repository that are not garbage.
    repoReleaseInfo :: t -> [Release]
    checkCompatibility :: t -> IO ()
    checkCompatibility repo =
        do level <- repositoryCompatibilityLevel repo
           case level of
             Nothing -> return ()
             Just n | n >= libraryCompatibilityLevel -> return ()
             Just n -> error ("Compatibility error: repository level " ++ show n ++
                              " < library level " ++ show libraryCompatibilityLevel ++ ", please upgrade.")

-- |The name of the file which holds the repository's compatibility
-- level.
compatibilityFile :: FilePath
compatibilityFile = "repository-compat"

-- | The compatibility level of this library and any applications
-- which use it.  It is an error if we try to use a repository whose
-- compatibility level is higher than this, a newer version of the
-- library must be used.  This value was increased from 1 to 2 due
-- to a new version number tagging policy.
libraryCompatibilityLevel :: Int
libraryCompatibilityLevel = 2

repoURI :: Repo r => r -> VendorURI
repoURI = repoKeyURI . repoKey

repoKeyURI :: RepoKey -> VendorURI
repoKeyURI (Local path) = review vendorURI . fromJust . parseURI $ "file://" ++ view envPath path
repoKeyURI (Remote uri) = review vendorURI $ fromURI' uri

repoArchList :: Repo r => r -> Set Arch
repoArchList = unions . map releaseArchitectures . repoReleaseInfo
