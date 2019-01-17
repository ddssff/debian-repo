-- | Manage state information about the available repositories,
-- releases, OS images, and Apt images.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Repo.Internal.Repos
    ( MonadRepos(getRepos, putRepos)
    , ReposState
    , runReposT

    , putOSImage
    , osFromRoot

    , AptKey
    , getApt
    , getAptKey
    , putAptImage
    , evalMonadApt

    , repoByURI
    , putRepo

    , ReleaseKey
    , findRelease
    , putRelease
    , releaseByKey

    , MonadReposCached
    , runReposCachedT

    , syncOS
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException)
import Control.Lens (makeLenses, view)
import Control.Monad (unless)
import Control.Monad.Catch (bracket, catch, MonadCatch, MonadMask)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (MonadIO(..), MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (lift)
import Data.Map as Map (empty, fromList, insert, lookup, Map, toList, union)
import Data.Maybe (fromMaybe)
import Debian.Release (ReleaseName)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.OSImage (OSImage(..), osRoot, syncOS')
import Debian.Repo.MonadApt (AptImage, aptImageRoot)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (Repo, repoKey, RepoKey(..))
import Debian.Repo.Top (HasTop, MonadTop, sub)
import Debian.URI (URI')
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Files as F (removeLink)

data ReleaseKey = ReleaseKey RepoKey ReleaseName deriving (Eq, Ord, Show)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

-- | This represents the state of the IO system.
data ReposState
    = ReposState
      { _repoMap :: Map.Map URI' RemoteRepository                -- ^ Map to look up known (remote) Repository objects
      , _releaseMap :: Map.Map ReleaseKey Release -- ^ Map to look up known Release objects
      , _aptImageMap :: Map.Map AptKey AptImage  -- ^ Map to look up prepared AptImage objects
      , _osImageMap :: Map.Map EnvRoot OSImage   -- ^ Map to look up prepared OSImage objects
      }

$(makeLenses ''ReposState)

runReposT :: Monad m => StateT ReposState m a -> m a
runReposT action = (runStateT action) initState >>= return . fst

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: ReposState
initState = ReposState
            { _repoMap = Map.empty
            , _releaseMap = Map.empty
            , _aptImageMap = Map.empty
            , _osImageMap = Map.empty
            }

-- | A monad to support the IO requirements of the autobuilder.
class (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m) => MonadRepos m where
    getRepos :: m ReposState
    putRepos :: ReposState -> m ()

instance (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m) => MonadRepos (StateT ReposState m) where
    getRepos = get
    putRepos = put

{-
instance (MonadRepos m, Functor m) => MonadRepos (StateT s m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos
-}

instance MonadRepos m => MonadRepos (StateT EnvRoot m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

modifyRepos :: MonadRepos m => (ReposState -> ReposState) -> m ()
modifyRepos f = getRepos >>= putRepos . f

-- | Like @MonadRepos@, but is also an instance of MonadTop and tries to
-- load and save a list of cached repositories from @top/repoCache@.
class (MonadRepos m, MonadTop r m, MonadCatch m) => MonadReposCached r m

type ReposCachedT r m = ReaderT r (StateT ReposState m)

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runReposCachedT :: (MonadIO m, MonadFail m, MonadMask m, HasTop r) => r -> ReposCachedT r m a -> m a
runReposCachedT top action = do
  qPutStrLn "Running MonadReposCached..."
  r <- runReposT $ runReaderT (bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)) top
  qPutStrLn "Exited MonadReposCached..."
  return r

instance (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m, HasTop r) => MonadReposCached r (ReposCachedT r m)

instance (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m) => MonadRepos (ReposCachedT r m) where
    getRepos = lift get
    putRepos = lift . put

putOSImage :: MonadRepos m => OSImage -> m ()
putOSImage repo = modifyRepos (\s -> s {_osImageMap = Map.insert (view osRoot repo) repo (view osImageMap s)})
--putOSImage repo = modifyRepos (\s -> set osImageMap (Map.insert (view osRoot repo) repo (view osImageMap s)) s)

osFromRoot :: MonadRepos m => EnvRoot -> m (Maybe OSImage)
osFromRoot root = Map.lookup root . view osImageMap <$> getRepos

putRepo :: MonadRepos m => URI' -> RemoteRepository -> m ()
putRepo uri repo = modifyRepos (\ s -> s {_repoMap = Map.insert uri repo (view repoMap s)})

repoByURI :: MonadRepos m => URI' -> m (Maybe RemoteRepository)
repoByURI uri = Map.lookup uri . view repoMap <$> getRepos

getApt :: MonadRepos m => EnvRoot -> m (Maybe AptImage)
getApt root = (Map.lookup (AptKey root) . view aptImageMap) <$> getRepos

getAptKey :: MonadRepos m => EnvRoot -> m (Maybe AptKey)
getAptKey root = fmap (AptKey . view aptImageRoot) <$> (getApt root)

findRelease :: (Repo r, MonadRepos m) => r -> ReleaseName -> m (Maybe Release)
findRelease repo dist = (Map.lookup (ReleaseKey (repoKey repo) dist) . view releaseMap) <$> getRepos

releaseByKey :: MonadRepos m => ReleaseKey -> m Release
releaseByKey key = do
  Just rel <- (Map.lookup key . view releaseMap) <$> getRepos
  return rel

putRelease :: (Repo r, MonadRepos m) => r -> Release -> m ReleaseKey
putRelease repo release = do
    let key = ReleaseKey (repoKey repo) (releaseName release)
    modifyRepos (\ s -> s {_releaseMap = Map.insert key release (view releaseMap s)})
    return key

putAptImage :: MonadRepos m => AptImage -> m AptKey
putAptImage repo = do
  let key = AptKey (view aptImageRoot repo)
  modifyRepos (\ s -> s {_aptImageMap = Map.insert key repo (view aptImageMap s)})
  return key

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadApt :: MonadRepos m => StateT AptImage m a -> AptKey -> m a
evalMonadApt task (AptKey key) = do
  Just apt <- getApt key
  (a, apt') <- runStateT task apt
  _ <- putAptImage apt'
  return a

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: MonadReposCached r m => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       mp <- liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty))
       modifyRepos (\ s -> s {_repoMap = mp})
    where
      loadRepoCache' :: FilePath -> IO (Map URI' RemoteRepository)
      loadRepoCache' repoCache =
          do qPutStrLn "Loading repo cache..."
             cache <- readFile repoCache
             case maybeRead cache of
               Nothing -> do
                   liftIO $ hPutStrLn stderr ("Removing invalid repoCache: " ++ show repoCache ++ " -> " ++ show cache)
                   removeFile repoCache
                   return mempty
               Just pairs ->
                   qPutStrLn ("Loaded " ++ show (length pairs) ++ " entries from the repo cache in " ++ show repoCache) >>
                   return (fromList pairs)

-- | Write the repo cache map into a file.
saveRepoCache :: MonadReposCached r m => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- view repoMap <$> getRepos
             repoCache <- liftIO $ loadCache path
             let merged = Map.union live repoCache
             liftIO (F.removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e)) >>
                     writeFile path (show . Map.toList $ merged))
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

syncOS :: (MonadTop r m, MonadRepos m) => OSImage -> EnvRoot -> m OSImage
syncOS srcOS dstRoot = do
  dstOS <- liftIO $ syncOS' srcOS dstRoot
  putOSImage dstOS
  dstOS' <- osFromRoot dstRoot
  maybe (error ("syncOS failed for " ++ show dstRoot)) return dstOS'
