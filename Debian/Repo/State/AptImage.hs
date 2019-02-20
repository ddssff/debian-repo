{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Debian.Repo.State.AptImage
    ( withAptImage
    , aptSourcePackages
    , aptBinaryPackages
    , prepareSource
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (SomeException)
import Control.Lens (at, set, use, view)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
--import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadIO(..))
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Debian.Changes (ChangeLogEntry(logVersion))
import Distribution.Pretty (prettyShow)
import Debian.Relation (SrcPkgName(unSrcPkgName))
import Debian.Repo.AptImage (aptDir, aptGetSource, aptGetUpdate)
import Debian.Repo.AptKey (AptKey(AptKey), MonadApt)
import Debian.Repo.EnvPath (rootPath)
import Debian.Repo.MonadApt (aptImageArch, aptImageRoot, aptImageSources,
                                 aptBinaryPackageCache, aptSourcePackageCache,
                                 cacheRootDir, createAptImage)
import Debian.Repo.MonadRepos (aptImageMap, getApt, modifyApt, MonadRepos, putAptImage, reposState)
import Debian.Repo.PackageID (PackageID(packageName), PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
--import Debian.Repo.Prelude (symbol)
import Debian.Repo.Prelude.Verbosity (qPutStr, qPutStrLn)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), SourcesChangedAction)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), HasChangeLog(entry), findDebianBuildTrees, SourceTree(dir'))
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (updateCacheSources)
import Debian.Repo.Top (MonadTop)
import Debian.TH (here, Loc)
import Debian.Version (DebianVersion)
import Extra.Except -- (HasIOException)
import System.Directory (createDirectoryIfMissing)
import System.Unix.Directory (removeRecursiveSafely)

#if 0
instance MonadApt m => MonadApt (StateT EnvRoot m) where
    getApt = lift getApt
    putApt = lift . putApt
#endif
#if 0
instance MonadRepos s m => MonadRepos (StateT AptImage m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos
#endif

withAptImage ::
    (MonadIOError e m, HasLoc e, MonadCatch m, MonadRepos s m, MonadTop r m)
    => [Loc]
    -> SourcesChangedAction
    -> NamedSliceList
    -> ReaderT (r, AptKey) m a
    -> m a
withAptImage locs sourcesChangedAction sources action = prepareAptImage ($here : locs) sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage ::
    forall r s e m. (MonadIOError e m, HasLoc e, MonadCatch m, MonadTop r m, MonadRepos s m)
    => [Loc]
    -> SourcesChangedAction   -- What to do if environment already exists and sources.list is different
    -> NamedSliceList         -- The sources.list
    -> m AptKey               -- The resulting environment
prepareAptImage locs sourcesChangedAction sources = do
  key <- {-getAptKey =<<-} AptKey <$> cacheRootDir (sliceListName sources)
  mimg <- use (reposState . aptImageMap . at key)
  case mimg of
    Nothing -> prepareAptImage' ($here : locs) sourcesChangedAction sources
    Just _ -> return key

-- | Create a new AptImage and insert it into the ReposState.
prepareAptImage' ::
    forall r s e m. (MonadIOError e m, HasLoc e, MonadCatch m, MonadTop r m, MonadRepos s m)
    => [Loc] -> SourcesChangedAction -> NamedSliceList -> m AptKey
prepareAptImage' locs sourcesChangedAction sources = do
  key <- {-getAptKey =<<-} AptKey <$> cacheRootDir (sliceListName sources)
  mimg <- use (reposState . aptImageMap . at key)
  case mimg of
    Nothing -> prepareAptImage'' `catch` handle
    Just _ -> return key
    where
      handle :: SomeException -> m AptKey
      handle e = do
        qPutStrLn ("Exception preparing " ++ (prettyShow . sliceListName $ sources) ++ ": " ++ show e)
        removeAptImage
        prepareAptImage''
      prepareAptImage'' = do
        qPutStrLn ("prepareAptImage' - sources=" ++ show sources <> " at " <> prettyShow ($here : locs))
        (key :: AptKey) <- putAptImage =<< createAptImage sources
        evalMonadApt (updateCacheSources ($here : locs) sourcesChangedAction sources >> aptGetUpdate) key
        return key
      removeAptImage = cacheRootDir (sliceListName sources) >>= liftIO . removeRecursiveSafely . view rootPath

evalMonadApt :: (MonadRepos s m, MonadTop r m) => ReaderT (r, AptKey) m a -> AptKey -> m a
evalMonadApt task key = do
  r <- ask
  runReaderT task (r, key)

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
-- updateAptEnv :: (MonadRepos s m, MonadApt m) => m ()
-- updateAptEnv = aptGetUpdate

aptSourcePackages :: (MonadIOError e m, HasLoc e, MonadRepos s m, MonadApt r m) => m [SourcePackage]
aptSourcePackages = do
  mpkgs <- view aptSourcePackageCache <$> getApt
  maybe aptSourcePackages' return mpkgs
    where
      aptSourcePackages' = do
        root <- view aptImageRoot <$> getApt
        arch <- view aptImageArch <$> getApt
        sources <- view aptImageSources <$> getApt
        -- qPutStrLn (prettyShow $here <> " aptSourcePackages (sliceListName sources)=" ++ prettyShow (sliceListName sources))
        pkgs <- sourcePackagesFromSources root arch (sliceList sources)
        modifyApt (set aptSourcePackageCache (Just pkgs))
        return pkgs

aptBinaryPackages :: (MonadIOError e m, HasLoc e, MonadRepos s m, MonadApt r m) => m [BinaryPackage]
aptBinaryPackages = do
  mpkgs <- view aptBinaryPackageCache <$> getApt
  maybe aptBinaryPackages' return mpkgs
    where
      aptBinaryPackages' = do
        root <- view aptImageRoot <$> getApt
        arch <- view aptImageArch <$> getApt
        sources <- view aptImageSources <$> getApt
        -- qPutStrLn ($(symbol 'aptBinaryPackages) ++ " " ++ show (pretty (sliceListName sources)))
        pkgs <- binaryPackagesFromSources root arch (sliceList sources)
        modifyApt (set aptBinaryPackageCache (Just pkgs))
        return pkgs

-- |Retrieve a source package via apt-get.
prepareSource :: (MonadIOError e m, HasLoc e, MonadCatch m, MonadRepos s m, MonadApt r m, MonadTop r m) =>
                 SrcPkgName                     -- The name of the package
              -> Maybe DebianVersion            -- The desired version, if Nothing get newest
              -> m DebianBuildTree              -- The resulting source tree
prepareSource package version =
    do root <- view (aptImageRoot . rootPath) <$> getApt
       dir <- aptDir package
       liftIO $ createDirectoryIfMissing True dir
       ready <- liftIO $ findDebianBuildTrees dir
       version' <- latestVersion package version
       case (version', ready) of
         (Nothing, _) ->
             fail $ "No available versions of " ++ unSrcPkgName package ++ " in " ++ root
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed in " ++ dir ++ " (1): trees=" ++ show (map (dir' . tree' . debTree') trees)
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                qPutStr $ "Retrieving APT source for " ++ unSrcPkgName package
                aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed (2): trees=" ++ show (map (dir' . tree' . debTree') trees)

-- | Find the most recent version of a source package.
latestVersion :: (MonadIOError e m, HasLoc e, MonadRepos s m, MonadApt r m) => SrcPkgName -> Maybe DebianVersion -> m (Maybe DebianVersion)
latestVersion package exact = do
  pkgs <- aptSourcePackages
  let versions = map (packageVersion . sourcePackageID) $ filter ((== package) . packageName . sourcePackageID) $ pkgs
      newest = listToMaybe $ reverse $ sort $ versions
  return $ maybe newest Just exact
