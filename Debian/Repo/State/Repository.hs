-- | A repository located on localhost
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.State.Repository
    ( readLocalRepository
    , prepareLocalRepository
    , prepareLocalRepository'
    , prepareRemoteRepository
    , repairLocalRepository
    , foldRepository
    ) where

import Control.Lens (review, view)
import Control.Monad (filterM, when)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Maybe (catMaybes)
import Debian.Codename (parseCodename)
import Debian.Except (HasIOException, liftEIO)
import Debian.Release (Section(Section))
import Debian.Repo.EnvPath (EnvPath, EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.LocalRepository (Layout(..), LocalRepository(..), readLocalRepo, repoRoot, repoLayout, repoReleaseInfoLocal)
import Debian.Repo.MonadRepos (MonadRepos, repoByURI, putRepo)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (getReleaseInfoRemote, parseArchitectures, Release(Release, releaseAliases, releaseArchitectures, releaseComponents, releaseName))
import Debian.Repo.RemoteRepository (RemoteRepository, RemoteRepository(RemoteRepository))
import Debian.Repo.Repo (RepoKey(..))
import Debian.TH (here, Loc)
import Debian.URI (fromURI', toURI', uriPathLens, uriSchemeLens)
import Debian.VendorURI (VendorURI, vendorURI)
import Distribution.Pretty (prettyShow)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Posix.Files as F (fileMode, getFileStatus, setFileMode)
import Text.Regex (matchRegex, mkRegex)

repairLocalRepository :: (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> LocalRepository -> m LocalRepository
repairLocalRepository locs r = prepareLocalRepository ($here : locs) (view repoRoot r) (view repoLayout r) (view repoReleaseInfoLocal r)

createLocalRepository :: (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> EnvPath -> Maybe Layout -> m (Maybe Layout)
createLocalRepository locs root layout = do
  --qPutStrLn ("createLocalRepository root=" <> show root <> " at " <> prettyShow ($here : locs))
  mapM_ (liftEIO locs . initDir)
            [(".", 0o40755),
             ("dists", 0o40755),
             ("incoming", 0o41755),
             ("removed", 0o40750),
             ("reject", 0o40750)]
  -- If repo exists compute its actual layout
  layout' <- liftEIO locs (computeLayout (outsidePath root)) >>= return . maybe layout Just
  -- >>= return . maybe (maybe (error "No layout specified for new repository") id layout) id
  mapM_ (liftEIO locs . initDir)
            (case layout' of
               Just Pool -> [("pool", 0o40755), ("installed", 0o40755)]
               Just Flat -> []
               Nothing -> [])
  return layout'
    where
      initDir (name, mode) =
          do let path = outsidePath root </> name
             filterM (\ f -> doesDirectoryExist f >>= return . not) [path] >>=
                     mapM_ (\ f -> createDirectoryIfMissing True f)
             actualMode <- F.getFileStatus path >>= return . F.fileMode
             when (mode /= actualMode) (F.setFileMode path mode)

readLocalRepository :: (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> EnvPath -> Maybe Layout -> m (Maybe LocalRepository)
readLocalRepository locs root layout =
    --qPutStrLn ("readLocalRepository root=" <> show root <> " at " <> prettyShow ($here : locs)) >>
    createLocalRepository ($here : locs) root layout >>= readLocalRepo root

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> EnvPath -> Maybe Layout -> [Release] -> m LocalRepository
prepareLocalRepository locs root layout releases =
    --qPutStrLn (prettyShow $here <> " - prepareLocalRepository root=" <> show root) >>
    readLocalRepository ($here : locs) root layout >>= maybe (return $ makeLocalRepo root layout releases) return

prepareLocalRepository' :: (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> EnvPath -> Maybe Layout -> m LocalRepository
prepareLocalRepository' locs root layout =
    prepareLocalRepository locs root layout [Release { releaseName = parseCodename "precise-seereason"
                                                , releaseAliases = []
                                                , releaseArchitectures = parseArchitectures "amd64, i386"
                                                , releaseComponents = [Section "main"] }]

makeLocalRepo :: EnvPath -> Maybe Layout -> [Release] -> LocalRepository
makeLocalRepo root layout releases =
    LocalRepository { _repoRoot = root
                    , _repoLayout = layout
                    , _repoReleaseInfoLocal = releases }

-- |Try to determine a repository's layout.
computeLayout :: FilePath -> IO (Maybe Layout)
computeLayout root =
    do
      -- If there are already .dsc files in the root directory
      -- the repository layout is Flat.
      isFlat <- getDirectoryContents root >>= return . (/= []) . catMaybes . map (matchRegex (mkRegex "\\.dsc$"))
      -- If the pool directory already exists the repository layout is
      -- Pool.
      isPool <- doesDirectoryExist (root ++ "/pool")
      case (isFlat, isPool) of
        (True, _) -> return (Just Flat)
        (False, True) -> return (Just Pool)
        _ -> return Nothing

prepareRemoteRepository :: (MonadIO m, MonadRepos s m, HasIOException e, MonadError e m) => [Loc] -> VendorURI -> m RemoteRepository
prepareRemoteRepository locs uri =
    repoByURI (toURI' (view vendorURI uri)) >>= maybe (loadRemoteRepository ($here : locs) uri) return

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
loadRemoteRepository :: (MonadIO m, MonadRepos s m, HasIOException e, MonadError e m) => [Loc] -> VendorURI -> m RemoteRepository
loadRemoteRepository locs uri =
    do releaseInfo <- liftEIO locs $ unsafeInterleaveIO $ getReleaseInfoRemote [$here] uri
       let repo = RemoteRepository (toURI' (view vendorURI uri)) releaseInfo
       putRepo (toURI' (view vendorURI uri)) repo
       return repo

-- foldRepository :: forall m r a. MonadState ReposState m => (r -> m a) -> RepoKey -> m a
-- foldRepository f key =
--     case key of
--       Local path -> prepareLocalRepository path Nothing >>= f
--       Remote uri' ->
--           let uri = fromURI' uri' in
--           case uriScheme uri of
--             "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= f
--             _ -> prepareRemoteRepository uri >>= f

foldRepository :: (MonadIO m, MonadRepos s m, HasIOException e, MonadError e m) => [Loc] -> (LocalRepository -> m a) -> (RemoteRepository -> m a) -> RepoKey -> m a
foldRepository locs f g key =
    case key of
      Local path -> readLocalRepository ($here : locs) path Nothing >>= maybe (error $ "No repository at " ++ show path) f
      Remote uri' ->
          let uri = review vendorURI (fromURI' uri') in
          case view (vendorURI . uriSchemeLens) uri of
            "file:" -> prepareLocalRepository' ($here : locs) (EnvPath (EnvRoot "") (view (vendorURI . uriPathLens) uri)) Nothing >>= f
            _ -> prepareRemoteRepository ($here : locs) uri >>= g
