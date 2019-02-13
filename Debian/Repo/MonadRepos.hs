-- | Manage state information about the available repositories,
-- releases, OS images, and Apt images.
{-# LANGUAGE ConstraintKinds, CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Repo.MonadRepos
    ( HasReposState(..)
    , MonadRepos, getRepos, putRepos
    , ReposState, repoMap, releaseMap, aptImageMap, osImageMap
    , runReposT

    , putOSImage
    --, osFromRoot

    , getApt
    , putApt
    , modifyApt
    -- , getAptKey
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
import Control.DeepSeq (NFData(..))
import Control.Exception (IOException)
import Control.Lens ((.=), at, Lens', makeLenses, use, view)
import Control.Monad (unless)
import Control.Monad.Catch (bracket, catch, MonadCatch, MonadMask)
--import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadIO(..), MonadState, StateT(runStateT))
--import Control.Monad.Trans (lift)
import Data.Map as Map (empty, fromList, insert, lookup, Map, toList, union)
import Data.Maybe (fromJust, fromMaybe)
import Debian.Codename (Codename)
import Debian.Except (HasIOException, MonadError)
import Debian.Repo.AptKey (AptKey(AptKey), HasAptKey(aptKey), MonadApt)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.OSImage (OSImage(..), osRoot, syncOS')
import Debian.Repo.OSKey ({-HasOSKey,-} OSKey(..))
import Debian.Repo.MonadApt (AptImage, aptImageRoot)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (Repo, repoKey, RepoKey(..))
import Debian.Repo.Top (HasTop, MonadTop, sub)
import Debian.URI (URI')
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Files as F (removeLink)

data ReleaseKey = ReleaseKey RepoKey Codename deriving (Eq, Ord, Show)

-- | This represents the state of the IO system.
data ReposState
    = ReposState
      { _repoMap :: Map.Map URI' RemoteRepository                -- ^ Map to look up known (remote) Repository objects
      , _releaseMap :: Map.Map ReleaseKey Release -- ^ Map to look up known Release objects
      , _aptImageMap :: Map.Map AptKey AptImage  -- ^ Map to look up prepared AptImage objects
      , _osImageMap :: Map.Map OSKey OSImage   -- ^ Map to look up prepared OSImage objects
      }

$(makeLenses ''ReposState)

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: ReposState
initState = ReposState
            { _repoMap = Map.empty
            , _releaseMap = Map.empty
            , _aptImageMap = Map.empty
            , _osImageMap = Map.empty
            }

class HasReposState s where reposState :: Lens' s ReposState
instance HasReposState ReposState where reposState = id
type MonadRepos s m = (HasReposState s, MonadState s m)

getRepos :: (HasReposState s, MonadState s m) => m ReposState
getRepos = use reposState

putRepos :: (HasReposState s, MonadState s m) => ReposState -> m ()
putRepos x = reposState .= x

modifyRepos :: (HasReposState s, MonadState s m) => (ReposState -> ReposState) -> m ()
modifyRepos f = getRepos >>= putRepos . f

type MonadReposCached r s m = (MonadRepos s m, MonadTop r m)

runReposT :: Monad m => StateT ReposState m a -> m a
runReposT action = (runStateT action) initState >>= return . fst

#if 0
-- | A monad to support the IO requirements of the autobuilder.
class (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m) => MonadRepos s m where
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

instance MonadRepos s m => MonadRepos (StateT EnvRoot m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

-- | Like @MonadRepos@, but is also an instance of MonadTop and tries to
-- load and save a list of cached repositories from @top/repoCache@.
class (MonadRepos s m, MonadTop r m, MonadCatch m) => MonadReposCached r m
#endif

type ReposCachedT r m = ReaderT r (StateT ReposState m)

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runReposCachedT ::
    (MonadError e m, MonadIO m, MonadMask m, HasTop r)
    => r
    -> ReposCachedT r m a
    -> m a
runReposCachedT top action = do
  qPutStrLn "Running MonadReposCached..."
  r <- runReposT $ runReaderT (bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)) top
  qPutStrLn "Exited MonadReposCached..."
  return r

#if 0
instance (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m, HasTop r) => MonadReposCached r (ReposCachedT r m)

instance (MonadCatch m, MonadMask m, MonadIO m, MonadFail m, Functor m) => MonadRepos (ReposCachedT r m) where
    getRepos = lift get
    putRepos = lift . put
#endif

putOSImage ::
    (HasReposState s, MonadState s m)
    => OSImage -> m ()
putOSImage repo = modifyRepos (\s -> s {_osImageMap = Map.insert (view osRoot repo) repo (view osImageMap s)})
--putOSImage repo = modifyRepos (\s -> set osImageMap (Map.insert (view osRoot repo) repo (view osImageMap s)) s)

--osFromRoot :: MonadRepos s m => EnvRoot -> m (Maybe OSImage)
--osFromRoot root = Map.lookup root . view osImageMap <$> getRepos

putRepo :: MonadRepos s m => URI' -> RemoteRepository -> m ()
putRepo uri repo = modifyRepos (\ s -> s {_repoMap = Map.insert uri repo (view repoMap s)})

repoByURI :: MonadRepos s m => URI' -> m (Maybe RemoteRepository)
repoByURI uri = Map.lookup uri . view repoMap <$> getRepos

getApt :: (MonadRepos s m, MonadApt r m) => m AptImage
getApt = do
  key <- view aptKey
  fromJust <$> use (reposState . aptImageMap . at key)

putApt :: MonadRepos s m => AptImage -> m ()
putApt img = do
  (reposState . aptImageMap . at (AptKey key)) .= Just img
    where key :: EnvRoot
          key = view aptImageRoot img

modifyApt :: (MonadRepos s m, MonadApt r m) => (AptImage -> AptImage) -> m ()
modifyApt f = getApt >>= putApt . f

#if 0
getAptKey :: MonadRepos s m => EnvRoot -> m (Maybe AptKey)
getAptKey root = fmap (AptKey . view aptImageRoot) <$> (getApt root)
#endif

findRelease :: (Repo r, MonadRepos s m) => r -> Codename -> m (Maybe Release)
findRelease repo dist = (Map.lookup (ReleaseKey (repoKey repo) dist) . view releaseMap) <$> getRepos

releaseByKey :: MonadRepos s m => ReleaseKey -> m Release
releaseByKey key = do
  ~(Just rel) <- (Map.lookup key . view releaseMap) <$> getRepos
  return rel

putRelease :: (Repo r, MonadRepos s m) => r -> Release -> m ReleaseKey
putRelease repo release = do
    let key = ReleaseKey (repoKey repo) (releaseName release)
    modifyRepos (\ s -> s {_releaseMap = Map.insert key release (view releaseMap s)})
    return key

putAptImage :: MonadRepos s m => AptImage -> m AptKey
putAptImage repo = do
  let key = AptKey (view aptImageRoot repo)
  modifyRepos (\ s -> s {_aptImageMap = Map.insert key repo (view aptImageMap s)})
  return key

evalMonadApt :: (MonadRepos s m, MonadReader r m) => ReaderT (AptKey, r) m a -> AptKey -> m a
evalMonadApt task key = do
  r <- ask
  runReaderT task (key, r)

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: (MonadIO m, MonadReposCached r s m) => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       mp <- liftIO (loadRepoCache' dir `catch` (\(e :: IOException) -> qPutStrLn (show e) >> return Map.empty))
       modifyRepos (\ s -> s {_repoMap = mp})
    where
      loadRepoCache' :: FilePath -> IO (Map URI' RemoteRepository)
      loadRepoCache' repoCache =
          do qPutStrLn "Loading repo cache..."
             cache <- readFile repoCache
             case maybeRead cache of
               Nothing -> do
                   liftIO $ hPutStrLn stderr ("Removing invalid repoCache: " ++ show repoCache ++ " -> " ++ take 200 (show cache))
                   removeFile repoCache
                   return mempty
               Just pairs ->
                   qPutStrLn ("Loaded " ++ show (length pairs) ++ " entries from the repo cache in " ++ show repoCache) >>
                   return (fromList pairs)

-- | Write the repo cache map into a file.
saveRepoCache :: (MonadIO m, MonadReposCached r s m) => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- view repoMap <$> getRepos
             repoCache <- liftIO $ loadCache path
             let merged = Map.union live repoCache
             liftIO (do F.removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e))
                        let s = show . Map.toList $ merged
                        rnf s `seq` writeFile path s)
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `catch` (\ (_ :: IOException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

syncOS ::
    forall e s m. (MonadIO m, MonadCatch m,
                   --MonadTop r m, HasOSKey r, {-MonadReader r m,-}
                   HasReposState s, MonadState s m,
                   HasIOException e, HasRsyncError e, MonadError e m)
    => OSImage -> OSKey -> m OSImage
syncOS srcOS dstRoot = do
  dstOS <- syncOS' srcOS dstRoot
  putOSImage dstOS
  dstOS' <- use (reposState . osImageMap . at dstRoot)
  maybe (error ("syncOS failed for " ++ show dstRoot)) return dstOS'
