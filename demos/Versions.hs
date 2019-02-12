{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
-- |Print the available version numbers of a package.
module Main where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromJust)
import Debian.Arch (Arch(Binary), ArchCPU(..), ArchOS(..))
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.MonadRepos (MonadRepos, runReposT, putRelease)
import Debian.Except (liftEIO)
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (RepoKey(Remote), repoReleaseInfo)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Repo.DebError (DebError)
import Debian.TH (here)
import Debian.URI (readURI')

main :: IO ()
main = do
  r <- runExceptT (runReposT main')
  case r of
    Left (e :: DebError) -> putStrLn ("Versions - exception " ++ show e) >> return ()
    Right () -> return ()

main' :: forall s e m. (MonadIO m, e ~ DebError, MonadError e m, MonadRepos s m) => m ()
main' =
    foldRepository f g (Remote (fromJust (readURI' uri)))
    where
      f :: LocalRepository -> m ()
      f repo =
          do releases <- mapM (putRelease repo) (repoReleaseInfo repo)
             liftEIO [$here] (putStrLn ("\n" ++ show releases))
      g :: RemoteRepository -> m ()
      g repo =
          do releases <- mapM (putRelease repo) (repoReleaseInfo repo)
             liftEIO [$here] (putStrLn ("\n" ++ show releases))
{-
    do repo <- prepareRepository (Remote (fromJust (readURI' uri)))
       releases <- mapM (insertRelease repo) (repoReleaseInfo repo)
       -- let binaryIndexes = map (filter (\ i -> packageIndexArch i == arch)) (map binaryIndexList releases)
       -- _binaryPackages <- mapM (packageLists release) binaryIndexes
       liftIO (putStrLn ("\n" ++ show releases))
-}
{-
    where
      insert repo info = insertRelease repo 
-}

-- packageLists :: MonadRepos m => Release -> [PackageIndex] -> m [[BinaryPackage]]
-- packageLists release indexes = mapM (packages release) indexes

-- packages :: MonadRepos m => Release -> PackageIndex -> m [BinaryPackage]
-- packages release index = liftIO (binaryPackagesOfIndex release index) >>= return . either throw id

uri = "http://deb.seereason.com/ubuntu/"
arch = Binary (ArchOS "linux") (ArchCPU "i386")
