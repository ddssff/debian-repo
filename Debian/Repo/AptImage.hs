{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( aptDir
    , buildArchOfRoot
    , aptGetSource
    , aptGetUpdate
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Category ((.))
import Control.Lens (view)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString as B
import Data.Data (Data)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Pretty (ppShow)
import Debian.Relation (PkgName, SrcPkgName(unSrcPkgName))
import Debian.Repo.EnvPath (EnvRoot(..), rootPath)
import Debian.Repo.MonadApt (AptImage, aptImageRoot, aptImageSources, MonadApt)
import Debian.Repo.MonadRepos (getApt, MonadRepos)
import Debian.Repo.Prelude.Process (readProcessV, readProcessQE)
import Debian.Repo.Slice (NamedSliceList(sliceListName))
import Debian.Repo.Top (distDir, MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), proc, readProcessWithExitCode)

-- | The location of the top directory of a source packages's files in
-- an AptImage (but not an OSImage.)
aptDir :: (MonadRepos s m, MonadTop r m, MonadApt r m) => SrcPkgName -> m FilePath
aptDir package =
    do rel <- view aptImageSources <$> getApt
       dir <- distDir (sliceListName rel)
       return $ dir </> "apt" </> unSrcPkgName package

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetSource :: (MonadRepos s m, MonadApt r m, PkgName n) => FilePath -> [(n, Maybe DebianVersion)] -> m ()
aptGetSource dir packages =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["source"] ++ map formatPackage packages)) {cwd = Just dir}
       liftIO $ createDirectoryIfMissing True dir >> readProcessV p B.empty >> return ()
    where
      formatPackage (name, Nothing) = ppShow name
      formatPackage (name, Just version) = ppShow name ++ "=" ++ show (prettyDebianVersion version)

aptGetUpdate :: (MonadRepos s m, MonadApt r m) => m ()
aptGetUpdate =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["update"]))
       _ <- liftIO $ readProcessQE p B.empty
       return ()

aptOpts :: (MonadRepos s m, MonadApt r m) => m [String]
aptOpts =
    do root <- view (aptImageRoot . rootPath) <$> getApt
       return $ [ "-o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status"
                , "-o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists"
                , "-o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives"
                , "-o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list"
                , "-o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d" ]
