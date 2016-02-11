{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
module Debian.Repo.Rsync
    ( RsyncError(..)
    , rsync
    , rsyncOld
    ) where

import Control.Exception (Exception, SomeException, throw, toException, try)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Data.Typeable (Typeable)
import Debian.Repo.Prelude.Process (readProcessV)
import System.Exit (ExitCode(..))
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess, proc)
import System.Process.ListLike (readCreateProcess)

-- | Function that invokes rsync(1) with arguments insuring that copy
-- will become an exact copy of orig.
rsync :: [String] -- Additional rsync arguments
      -> FilePath -- Original directory
      -> FilePath -- Copy (desitination) directory
      -> IO ()
rsync extra orig copy = do
  let p = proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                        [dropTrailingPathSeparator orig ++ "/",
                         dropTrailingPathSeparator copy])
  result <- try $ readProcessV p mempty :: IO (Either RsyncError (ExitCode, ByteString, ByteString))
  case result of
    Right (code, out, err) -> maybe (return ()) (throw . toException) (buildRsyncError p code)
    Left e -> throw e

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
    | RsyncUnexpected Int
    deriving (Typeable, Show)

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

-- | For backwards compatibility
rsyncOld :: forall m. (Functor m, MonadIO m) =>
            [String] -- Additional rsync arguments
         -> FilePath -- Original directory
         -> FilePath -- Copy (desitination) directory
         -> m (Either SomeException ExitCode, String, String)
rsyncOld extra orig copy =
    liftIO $ try (rsync extra orig copy) >>= return . either (\ (e :: SomeException) -> (Left e, mempty, mempty)) (\ () -> (Right ExitSuccess, mempty, mempty))
