{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}
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
import Control.Lens (set, view)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadIO(..), MonadTrans(lift))
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Pretty (prettyShow)
import Debian.Relation (SrcPkgName(unSrcPkgName))
import Debian.Repo.AptImage (aptDir, aptGetSource, aptGetUpdate)
import Debian.Repo.EnvPath (EnvRoot, rootPath)
import Debian.Repo.MonadApt (AptImage, aptImageArch, aptImageRoot, aptImageSources,
                                 aptBinaryPackageCache, aptSourcePackageCache,
                                 cacheRootDir, createAptImage, MonadApt(..), modifyApt)
import Debian.Repo.MonadRepos (AptKey, evalMonadApt, getAptKey, MonadRepos(..), putAptImage)
import Debian.Repo.PackageID (PackageID(packageName), PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Prelude (symbol)
import Debian.Repo.Prelude.Verbosity (qPutStr, qPutStrLn)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), SourcesChangedAction)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), HasChangeLog(entry), findDebianBuildTrees, SourceTree(dir'))
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (updateCacheSources)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Unix.Directory (removeRecursiveSafely)

instance MonadApt m => MonadApt (StateT EnvRoot m) where
    getApt = lift getApt
    putApt = lift . putApt

instance MonadRepos m => MonadRepos (StateT AptImage m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

withAptImage :: (MonadRepos m, MonadTop r m) => SourcesChangedAction -> NamedSliceList -> StateT AptImage m a -> m a
withAptImage sourcesChangedAction sources action = prepareAptImage sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage :: forall r m. (MonadTop r m, MonadRepos m) =>
                 SourcesChangedAction   -- What to do if environment already exists and sources.list is different
              -> NamedSliceList         -- The sources.list
              -> m AptKey               -- The resulting environment
prepareAptImage sourcesChangedAction sources = do
  mkey <- getAptKey =<< cacheRootDir (sliceListName sources)
  maybe (prepareAptImage' sourcesChangedAction sources) return mkey

prepareAptImage' :: forall r m. (MonadCatch m, MonadTop r m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptKey
prepareAptImage' sourcesChangedAction sources = do
  mkey <- getAptKey =<< cacheRootDir (sliceListName sources)
  maybe (prepareAptImage'' `catch` handle) return mkey
    where
      handle :: SomeException -> m AptKey
      handle e = do
        qPutStrLn ("Exception preparing " ++ (prettyShow . sliceListName $ sources) ++ ": " ++ show e)
        removeAptImage
        prepareAptImage''
      prepareAptImage'' = do
        qPutStrLn ($(symbol 'prepareAptImage) ++ ": " ++ (prettyShow . sliceListName $ sources))
        key <- putAptImage =<< createAptImage sources
        evalMonadApt (updateCacheSources sourcesChangedAction sources >> aptGetUpdate) key
        return key
      removeAptImage = cacheRootDir (sliceListName sources) >>= liftIO . removeRecursiveSafely . view rootPath

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
-- updateAptEnv :: (MonadRepos m, MonadApt m) => m ()
-- updateAptEnv = aptGetUpdate

aptSourcePackages :: (MonadRepos m, MonadApt m) => m [SourcePackage]
aptSourcePackages = do
  mpkgs <- view aptSourcePackageCache <$> getApt
  maybe aptSourcePackages' return mpkgs
    where
      aptSourcePackages' = do
        root <- view aptImageRoot <$> getApt
        arch <- view aptImageArch <$> getApt
        sources <- view aptImageSources <$> getApt
        -- qPutStrLn ($(symbol 'aptSourcePackages) ++ " " ++ show (pretty (sliceListName sources)))
        pkgs <- sourcePackagesFromSources root arch (sliceList sources)
        modifyApt (set aptSourcePackageCache (Just pkgs))
        return pkgs

aptBinaryPackages :: (MonadRepos m, MonadApt m) => m [BinaryPackage]
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
prepareSource :: (MonadRepos m, MonadApt m, MonadTop r m) =>
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
latestVersion :: (MonadRepos m, MonadApt m) => SrcPkgName -> Maybe DebianVersion -> m (Maybe DebianVersion)
latestVersion package exact = do
  pkgs <- aptSourcePackages
  let versions = map (packageVersion . sourcePackageID) $ filter ((== package) . packageName . sourcePackageID) $ pkgs
      newest = listToMaybe $ reverse $ sort $ versions
  return $ maybe newest Just exact
