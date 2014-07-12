{-# LANGUAGE FlexibleInstances #-}
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (MonadIO)
import Data.Map as Map (Map, empty)
import Debian.Repo.Monads.Top (TopT, runTopT)
import Debian.Repo.EnvPath (rootEnvPath)
import Debian.Repo.Repo (RepoKey)
import Debian.Repo.Repository (MonadRepoCache(..), Repository, loadRepoCache)

root = rootEnvPath "/srv/deb/ubuntu"
home = "/home/dsf/.autobuilder"

instance MonadRepoCache (StateT (Map RepoKey Repository) (TopT IO)) where
    getRepoCache = get
    putRepoCache = put

main = runTopT home (runStateT test Map.empty)

test = loadRepoCache :: StateT (Map RepoKey Repository) (TopT IO) ()
