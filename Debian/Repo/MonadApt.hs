{-# LANGUAGE ConstraintKinds, CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadApt
    ( AptImage(..)
    , aptImageRoot, aptImageArch, aptImageSources
    , aptSourcePackageCache, aptBinaryPackageCache
    , AptKey(AptKey)
    , HasAptKey(aptKey)
    , MonadApt --, getApt, putApt, modifyApt
    , cacheRootDir
    , createAptImage
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Category ((.))
import Control.Lens (_1, _2, Getter, makeLenses, view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState(get, put), StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import Debian.Arch (Arch)
import Debian.Pretty (prettyShow)
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(EnvRoot), rootPath)
import Debian.Repo.IO (buildArchOfRoot)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (replaceFile, writeFileIfMissing)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName))
import Debian.Repo.Top (dists, MonadTop)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data AptImage =
    AptImage { _aptImageRoot :: EnvRoot
             , _aptImageArch :: Arch
             , _aptImageSources :: NamedSliceList
             , _aptSourcePackageCache :: Maybe [SourcePackage]
             , _aptBinaryPackageCache :: Maybe [BinaryPackage]
             }

$(makeLenses ''AptImage)

#if 0
class (Monad m, Functor m) => MonadApt m where
    getApt :: m AptImage
    putApt :: AptImage -> m ()

instance (Monad m, Functor m) => MonadApt (StateT AptImage m) where
    getApt = get
    putApt = put

modifyApt :: MonadApt m => (AptImage -> AptImage) -> m ()
modifyApt f = getApt >>= putApt . f
#else
newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

class HasAptKey r where aptKey :: Getter r AptKey
instance HasAptKey AptKey where aptKey = id
type MonadApt r m = (HasAptKey r, MonadReader r m)

instance HasAptKey (a, AptKey) where aptKey = _2
instance HasAptKey (a, AptKey, b) where aptKey = _2
#endif

instance Show AptImage where
    show apt = "AptImage " ++ relName (sliceListName (view aptImageSources apt))

instance Ord AptImage where
    compare a b = compare (sliceListName . view aptImageSources $ a) (sliceListName . view aptImageSources $ b)

instance Eq AptImage where
    a == b = compare a b == EQ

createAptImage :: (MonadTop r m, MonadIO m) => NamedSliceList -> m AptImage
createAptImage sources = do
  root <- cacheRootDir (sliceListName sources)
  liftIO $ do
    arch <- buildArchOfRoot
    let apt = AptImage { _aptImageRoot = root
                       , _aptImageArch = arch
                       , _aptImageSources = sources
                       , _aptSourcePackageCache = Nothing
                       , _aptBinaryPackageCache = Nothing }

    --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
    createDirectoryIfMissing True (view rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (view rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (view rootPath root ++ "/var/cache/apt/archives/partial")
    createDirectoryIfMissing True (view rootPath root ++ "/var/lib/dpkg")
    createDirectoryIfMissing True (view rootPath root ++ "/etc/apt")
    writeFileIfMissing True (view rootPath root ++ "/var/lib/dpkg/status") ""
    writeFileIfMissing True (view rootPath root ++ "/var/lib/dpkg/diversions") ""
    -- We need to create the local pool before updating so the
    -- sources.list will be valid.
    let sourceListText = prettyShow (sliceList sources)
    -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
    replaceFile (view rootPath root ++ "/etc/apt/sources.list") sourceListText
    return apt

cacheRootDir :: MonadTop r m => ReleaseName -> m EnvRoot
cacheRootDir release = EnvRoot . (</> relName release </> "aptEnv") <$> dists
