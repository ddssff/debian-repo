{-# LANGUAGE ConstraintKinds, CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS, getOS, putOS, modifyOS
    , evalMonadOS, evalMonadOS'
    , updateLists
    , aptGetInstall
    , syncLocalPool
    , osFlushPackageCache
    , buildEssential
    , Debian.Repo.MonadOS.syncOS
    ) where

--import Control.Applicative ((<|>), Alternative)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Lens (at, set, to, use, view)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask {-mask_-}, try)
import Control.Monad.Except (ExceptT, lift, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Except () -- instances
import Data.ByteString.Lazy as L (ByteString, empty)
--import Data.String (IsString)
--import Data.Traversable
import Debian.Pretty (ppShow)
import Debian.Except (HasIOException(fromIOException), liftEIO)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(..), rootPath)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.MonadRepos (HasReposState, MonadRepos, osImageMap, putOSImage, reposState, syncOS)
import Debian.Repo.Mount (withProcAndSys)
import Debian.Repo.OSImage as OS (OSImage(..), osRoot, osLocalMaster, osLocalCopy)
import Debian.Repo.OSKey (OSKey(..), HasOSKey(..))
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude.Process (runV2{-, showCommandAndResult-})
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.Top (MonadTop)
import Debian.TH (here)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Language.Haskell.TH.Syntax (Loc)
import Prelude hiding (mapM, sequence)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (proc)
--import System.Process.ListLike (ListLikeProcessIO, showCreateProcessForUser)
import System.Unix.Chroot (useEnv)

-- import System.Unix.Mount (withProcAndSys, WithProcAndSys)

-- | The problem with having an OSImage in the state of MonadOS is
-- that then we are modifying a copy of the OSImage in MonadRepos, we
-- want to go into MonadRepos and modify the map element there.  So
-- instead put an EnvRoot to look up the OSImage.
type MonadOS r s m = (HasOSKey r, MonadReader r m, MonadRepos s m)

getOS ::
    (MonadIO m, HasIOException e, MonadError e m,
     HasOSKey r, MonadReader r m,
     HasReposState s, MonadState s m)
    => m OSImage
getOS = do
  key <- view osKey
  repo <- use (reposState . osImageMap . at key)
  maybe (throwError (fromIOException [$here] (userError "getOS"))) return repo

putOS :: (HasReposState s, MonadState s m) => OSImage -> m ()
putOS = putOSImage

modifyOS ::
    (MonadIO m, HasIOException e, MonadError e m,
     HasOSKey r, MonadReader r m,
     HasReposState s, MonadState s m)
    => (OSImage -> OSImage) -> m ()
modifyOS f = getOS >>= putOS . f

-- | Perform a task in the changeroot of an OS.
useOS ::
    forall r s e m a. (MonadIO m, MonadMask m,
                       HasIOException e, MonadError e m,
                       HasOSKey r, MonadReader r m,
                       HasReposState s, MonadState s m)
    => [Loc] -> m a -> m a
useOS locs action =
  do root <- view (osRoot . to _root . rootPath) <$> getOS
     result <- try (withProcAndSys [$here] root (useEnv root (liftIO . evaluate) action))
     either (throwError . fromIOException ($here : locs)) return result

-- useEnv :: (MonadIO m, MonadMask m) => FilePath -> (a -> m a) -> m a -> m a

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadReader r m => ReaderT (r, OSKey) m a -> OSKey -> m a
evalMonadOS task key = ask >>= \r -> runReaderT task (r, key)

evalMonadOS' :: ReaderT OSKey m a -> OSKey -> m a
evalMonadOS' task root = runReaderT task root

tryExceptT :: MonadError e m  => ExceptT e m a -> ExceptT e m (Either e a)
tryExceptT = lift . runExceptT

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists ::
    forall r s e m. (MonadIO m, MonadMask m, HasReposState s, MonadState s m,
                     HasOSKey r, MonadReader r m,
                     HasIOException e, MonadError e m, Show e) => ExceptT e m ()
updateLists = do
  r1 <- tryExceptT (ecode_ update)
  case r1 of
    Right () -> ecode_ upgrade
    Left e -> do
      qPutStrLn ("exception: " ++ show e)
      r2 <- tryExceptT (ecode_ aptinstall >> ecode_ configure >> ecode_ update)
      case r2 of
        Right () -> ecode_ upgrade
        Left e' -> throwError e'
{-
  case r1 of
    Right () -> upda
    Left e -> runExceptT (cmd_ aptinstall >> cmd_ configure >> cmd_ update)
  r2 <- if r1 then pure True else or <$> sequence [aptinstall >>= f, configure >>= f, update >>= f]
  if r2 then mask_ upgrade >>= f else pure False
-}
    where
      -- Turn ExitFailure into an error of type e
      ecode ::
             ExceptT e m (ExitCode, ByteString, ByteString)
          -> ExceptT e m (ExitCode, ByteString, ByteString)
      ecode cmd = do
        output <- cmd
        case output of
          (ExitSuccess, _, _) -> return output
          _ -> throwError (fromIOException [$here] (userError (show output)))
      -- Like ecode but discard the output
      ecode_ :: ExceptT e m (ExitCode, ByteString, ByteString) -> ExceptT e m ()
      ecode_ = void . ecode

      update :: ExceptT e m (ExitCode, ByteString, ByteString)
      update = do
        -- root <- view (osRoot . to _root . rootPath) <$> getOS
        output <- useOS [$here] (runV2 [$here] (proc "apt-get" ["update"]) L.empty)
        --qPutStrLn (prettyShow [$here] <> " - output: " ++ show output)
        return output
      aptinstall :: ExceptT e m (ExitCode, ByteString, ByteString)
      aptinstall = useOS [$here] (runV2 [$here] (proc "apt-get" ["-f", "--yes", "install"]) L.empty)
      configure :: ExceptT e m (ExitCode, ByteString, ByteString)
      configure = useOS [$here] (runV2 [$here] (proc "dpkg" ["--configure", "-a"]) L.empty)
      upgrade :: ExceptT e m (ExitCode, ByteString, ByteString)
      upgrade = useOS [$here] (runV2 [$here] (proc "apt-get" ["-f", "-y", "--force-yes", "dist-upgrade"]) L.empty)

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadIO m, MonadMask m, MonadOS r s m, PkgName n, HasIOException e, MonadError e m) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- view (osRoot . to _root . rootPath) <$> getOS
       withProcAndSys [$here] root $ useEnv root (return . force) $ do
         _ <- runV2 [$here] p L.empty
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = ppShow name
      formatPackage (name, Just version) = ppShow name ++ "=" ++ show (prettyDebianVersion version)

#if 0
-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
#endif

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: forall s r e m. (MonadIO m, MonadMask m,
                                  HasOSKey r, MonadReader r m,
                                  HasReposState s, MonadState s m,
                                  HasIOException e, HasRsyncError e, MonadError e m, Show e) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {_envRoot = view (osRoot . to _root) os, _envPath = "/work/localpool"}) (view osLocalMaster os)
       putOS (set osLocalCopy repo' os)
       -- Allow this to fail?  Was it failing before?  Ugh.
       _ <- runExceptT updateLists :: m (Either e ())
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache

osFlushPackageCache :: (MonadIO m, HasOSKey r, MonadReader r m, HasReposState s, MonadState s m, HasIOException e, MonadError e m) => m ()
osFlushPackageCache = modifyOS (\ os -> os {_osSourcePackageCache = Nothing, _osBinaryPackageCache = Nothing})

-- | Get the version of the newest ghc available in a build environment.
-- ghcNewestAvailableVersion :: (MonadIO m, Functor m, MonadState OSImage m) => m (Maybe DebianVersion)
-- ghcNewestAvailableVersion = do
--   root <- rootPath . osRoot <$> get
--   liftIO $ GHC.ghcNewestAvailableVersion root

buildEssential :: (MonadIO m, MonadOS r s m, HasIOException e, MonadError e m) => m Relations
buildEssential = getOS >>= liftEIO [$here] . OS.buildEssential

syncOS :: (MonadIO m, MonadCatch m, HasIOException e, HasRsyncError e, MonadError e m, HasOSKey r, MonadTop r m, HasReposState s, MonadState s m) => OSKey -> m ()
syncOS dstRoot =
    do srcOS <- getOS
       dstOS <- Debian.Repo.MonadRepos.syncOS srcOS dstRoot
       putOS dstOS
