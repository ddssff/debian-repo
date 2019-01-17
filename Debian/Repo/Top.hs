-- | The top of a directory which belongs to the client process, used
-- for temporary storage.  The autobuilder usually assigns the path
-- "~/.autobuilder", and stores build environments and downloaded
-- source here.
{-# LANGUAGE ConstraintKinds, CPP, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Debian.Repo.Top
    ( TopDir(TopDir)
    , HasTop(toTop)
    , MonadTop
    , sub
    , dists
    , distDir
    , sourcesPath
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Lens (Getter, view)
import Control.Monad.Reader (MonadReader)
import Debian.Release (ReleaseName(relName))
import System.FilePath ((</>), isRelative)

newtype TopDir = TopDir FilePath

class HasTop r where toTop :: Getter r TopDir
instance HasTop TopDir where toTop = id
type MonadTop r m = (HasTop r, MonadReader r m)

sub :: MonadTop r m => FilePath -> m FilePath
sub path | isRelative path = view toTop >>= \(TopDir top) -> return $ top </> path
sub path = fail ("sub - path argument must be relative: " ++ path)

dists :: MonadTop r m => m FilePath
dists = sub "dists"

-- | The directory in a repository where the package index files for a
-- particular dist or release is stored.  (Wait, that's not right.)
distDir :: MonadTop r m => ReleaseName -> m FilePath
distDir rel = (</> relName rel) <$> dists

-- | The path of the text file containing the sources.list (aka SliceList)
sourcesPath :: MonadTop r m => ReleaseName -> m FilePath
sourcesPath rel = (</> "sources") <$> distDir rel
