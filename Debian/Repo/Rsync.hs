{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
module Debian.Repo.Rsync
    ( RsyncError(..)
    , HasRsyncError(fromRsyncError)
    , rsync
    , rsyncOld
    , rsyncOld'
    ) where

import Control.Exception (Exception, IOException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, liftEither, MonadError, MonadIO, runExceptT, throwError, withExceptT)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.Typeable (Typeable)
import Debian.Except (HasIOException(fromIOException))
--import Debian.Repo.OSKey (HasOSKey)
import Debian.Repo.Prelude.Process (runV2)
import Debian.TH (here)
import Language.Haskell.TH.Syntax (Loc)
import System.Exit (ExitCode(..))
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess, proc)

-- | Function that invokes rsync(1) with arguments insuring that copy
-- will become an exact copy of orig.
rsync :: forall e r m. (e ~ RsyncError, MonadReader r m, MonadError e m, MonadIO m, MonadCatch m)
      => [String] -- Additional rsync arguments
      -> FilePath -- Original directory
      -> FilePath -- Copy (desitination) directory
      -> m ()
rsync extra orig copy = do
  let p = proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                        [dropTrailingPathSeparator orig ++ "/",
                         dropTrailingPathSeparator copy])
  result <- {-wrapIO-} (runV2 [$here] p mempty :: m (ExitCode, ByteString, ByteString))
  case result of
    (code, _out, _err) -> maybe (return ()) (throwError . fromRsyncError) (buildRsyncError p code)
    -- Left e -> throwError $ wrapIOException e

-- | Map the result code of a chunk list.
instance Exception RsyncError

data RsyncError
    = RsyncSuccess
    | RsyncSyntaxOrUsage
    | RsyncProtocolIncompatibility
    | RsyncErrorsSelectingInputOutputFiles
    | RsyncRequestedActionNotSupported
    | RsyncErrorStartingClientServerProtocol
    | RsyncDaemonUnableToAppendToLogfile
    | RsyncErrorInSocketIO
    | RsyncErrorInFileIO
    | RsyncErrorInRsyncProtocolDataStream
    | RsyncErrorsWithProgramDiagnostics
    | RsyncErrorInIPCCode
    | RsyncReceivedSIGUSR1orSIGINT
    | RsyncSomeErrorReturnedByWaitpid
    | RsyncErrorAllocatingCoreMemoryBuffers
    | PartialTransferDueToError
    | PartialTransferDueToVanishedSourceFiles
    | TheMaxDeleteLimitStoppedDeletions
    | TimeoutInDataSendReceive
    | TimeoutWaitingForDaemonConnection
    | RsyncIOException [Loc] IOException
    | RsyncUnexpected Int
    deriving (Typeable, Show, Eq, Ord)

instance Ord IOException where
    compare = compare `on` show

-- | Extract an rsync error constructor
buildRsyncError :: CreateProcess -> ExitCode -> Maybe RsyncError
buildRsyncError _ ExitSuccess = Nothing
buildRsyncError _ (ExitFailure n) = Just $ fst $ rsyncErrorInfo n

#if 0
rsyncError :: Int -> RsyncError
rsyncError = fst . rsyncErrorInfo

rsyncErrorMessage :: Int -> String
rsyncErrorMessage = snd . rsyncErrorInfo
#endif

rsyncErrorInfo :: Int -> (RsyncError, String)
rsyncErrorInfo 1  = (RsyncSyntaxOrUsage, "rsync: Syntax or usage error")
rsyncErrorInfo 2  = (RsyncProtocolIncompatibility, "rsync: Protocol incompatibility")
rsyncErrorInfo 3  = (RsyncErrorsSelectingInputOutputFiles, "rsync: Errors selecting input/output files, dirs")
rsyncErrorInfo 4  = (RsyncRequestedActionNotSupported, "rsync: Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server.")
rsyncErrorInfo 5  = (RsyncErrorStartingClientServerProtocol, "rsync: Error starting client-server protocol")
rsyncErrorInfo 6  = (RsyncDaemonUnableToAppendToLogfile, "rsync: Daemon unable to append to log-file")
rsyncErrorInfo 10 = (RsyncErrorInSocketIO, "rsync: Error in socket I/O")
rsyncErrorInfo 11 = (RsyncErrorInFileIO, "rsync: Error in file I/O")
rsyncErrorInfo 12 = (RsyncErrorInRsyncProtocolDataStream, "rsync: Error in rsync protocol data stream")
rsyncErrorInfo 13 = (RsyncErrorsWithProgramDiagnostics, "rsync: Errors with program diagnostics")
rsyncErrorInfo 14 = (RsyncErrorInIPCCode, "rsync: Error in IPC code")
rsyncErrorInfo 20 = (RsyncReceivedSIGUSR1orSIGINT, "rsync: Received SIGUSR1 or SIGINT")
rsyncErrorInfo 21 = (RsyncSomeErrorReturnedByWaitpid, "rsync: Some error returned by waitpid()")
rsyncErrorInfo 22 = (RsyncErrorAllocatingCoreMemoryBuffers, "rsync: Error allocating core memory buffers")
rsyncErrorInfo 23 = (PartialTransferDueToError, "Partial transfer due to error")
rsyncErrorInfo 24 = (PartialTransferDueToVanishedSourceFiles, "Partial transfer due to vanished source files")
rsyncErrorInfo 25 = (TheMaxDeleteLimitStoppedDeletions, "rsync: The --max-delete limit stopped deletions")
rsyncErrorInfo 30 = (TimeoutInDataSendReceive, "rsync: Timeout in data send/receive")
rsyncErrorInfo 35 = (TimeoutWaitingForDaemonConnection, "rsync: Timeout waiting for daemon connection")
rsyncErrorInfo n = (RsyncUnexpected n, "Unexpected rsync error: " ++ show n)

class HasRsyncError e where fromRsyncError :: RsyncError -> e
instance HasRsyncError RsyncError where fromRsyncError = id

instance HasIOException RsyncError where fromIOException locs = RsyncIOException locs
--instance HasWrappedIOException RsyncError where wrapIOException = RsyncIOException $here

-- | For backwards compatibility
#if 0
rsyncOld :: forall m. MonadIO m =>
            [String] -- Additional rsync arguments
         -> FilePath -- Original directory
         -> FilePath -- Copy (desitination) directory
         -> m (Either IOException ExitCode, String, String)
rsyncOld extra orig copy =
    (over _1 Right <$> rsync extra orig copy) `catchError` (\(RsyncIOException e :: RsyncError) -> (Left e, mempty, mempty))
    -- rsync extra orig copy >>= return . either (\ (e :: IOException) -> (Left e, mempty, mempty)) (\ () -> (Right ExitSuccess, mempty, mempty))
#else
rsyncOld :: forall e m. (MonadIO m, MonadCatch m, HasRsyncError e, MonadError e m)
      => [String] -- Additional rsync arguments
      -> FilePath -- Original directory
      -> FilePath -- Copy (desitination) directory
      -> m (Either IOException ExitCode, String, String)
rsyncOld extra orig copy = do
  let p = proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                        [dropTrailingPathSeparator orig ++ "/",
                         dropTrailingPathSeparator copy])
  result <- {-wrapIO-} (runV2 [$here] p mempty :: m (ExitCode, String, String))
  case result of
    (code, out, err) -> maybe (return (Right code, out, err)) (throwError . fromRsyncError) (buildRsyncError p code)
    -- Left e -> throwError $ wrapIOException e
#endif

rsyncOld' ::
    forall e m. (MonadIO m, MonadCatch m, HasRsyncError e, MonadError e m)
    => [String] -- Additional rsync arguments
    -> FilePath -- Original directory
    -> FilePath -- Copy (desitination) directory
    -> m (Either IOException ExitCode, String, String)
rsyncOld' extra orig copy =
    action'
    where
      action' :: m (Either IOException ExitCode, String, String)
      action' = liftEither =<< runExceptT (withExceptT fromRsyncError action)
      action :: forall m'. (MonadIO m', MonadCatch m'{-, MonadReader r m'-}) => ExceptT RsyncError m' (Either IOException ExitCode, String, String)
      action = rsyncOld extra orig copy
