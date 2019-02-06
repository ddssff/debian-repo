-- | Ensure that any OSKey or AptKey value available outside this
-- module corresponds to an existing entry in the corresponding
-- Map in ReposState.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Debian.Repo.IO
    ( buildArchOfRoot
    , HasIOException(fromIOException)
    , liftEIO
    ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (liftIO, MonadIO)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

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

class HasIOException e where fromIOException :: IOException -> e
instance HasIOException IOException where fromIOException = id

-- | Lift an IO operation into ExceptT FileError IO
liftEIO :: forall e m a. (MonadIO m, HasIOException e, MonadError e m) => IO a -> m a
liftEIO action =
    liftIO (try action) >>= either (\(e :: IOException) -> f e) return
    where f = throwError . fromIOException
