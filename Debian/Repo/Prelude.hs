{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, Rank2Types, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Repo.Prelude
    ( countTasks
    , nub'
    , (~=)
    , (%=)
    , symbol
    , partitionM
    , maybeWriteFile
    , replaceFile
    , cond
    , listIntersection
    , sameInode
    , isSublistOf
    , cd
    , cartesianProduct
    , writeFileIfMissing
    , getSubDirectories
    , dropPrefix
    ) where

import Control.Lens (Lens', over)
import Control.Monad.State (modify, MonadIO, MonadState)
import Data.List (group, sort)
import Data.List as List (map)
import Debian.Repo.Prelude.Bool (cond)
import Debian.Repo.Prelude.GPGSign (cd)
import Debian.Repo.Prelude.List (cartesianProduct, dropPrefix, isSublistOf, listIntersection, partitionM)
import Debian.Repo.Prelude.Misc (sameInode)
import Extra.Verbosity (ePutStrLn)
import Language.Haskell.TH (Exp(LitE), Lit(StringL), Name, nameBase, nameModule, Q)
import System.FilePath.Extra2 (getSubDirectories, maybeWriteFile, replaceFile, writeFileIfMissing)
import Text.Printf (printf)

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task

-- | This nub doesn't preserve order
nub' :: (Ord a) => [a] -> [a]
nub' = List.map head . group . sort

(~=) :: MonadState a m => Lens' a b -> b -> m ()
l ~= x = l %= const x

-- | Modify a value.  (This is a version of Data.Lens.Lazy.%= that returns () instead of a.)
(%=) :: MonadState a m => Lens' a b -> (b -> b) -> m ()
l %= f = modify (over l f)

-- | Build a string containing a symbol's fully qualified name (for debugging output.)
symbol :: Name -> Q Exp
symbol x = return $ LitE (StringL (maybe "" (++ ".") (nameModule x) ++ nameBase x))
