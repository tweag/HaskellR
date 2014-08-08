-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides helper functions and structures for safe memory
-- management when communicating with R.
--
--
module Language.R.GC
  ( preserve
  , preserveSome
  ) where

import Control.Memory.Region
import H.Internal.Prelude
import qualified Foreign.R as R
import System.Mem.Weak (addFinalizer)


-- | Store an object in Preserved region.
preserve :: MonadR m => R.SEXP s a -> m (R.SEXP G a)
preserve s = io $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject (R.unsafeRelease s'))
    return s'
  where
    s' = R.unsafeRelease s

-- | Store object in Preserved region
automaticSome :: MonadR m => R.SomeSEXP s -> m (R.SomeSEXP G)
automaticSome (SomeSEXP s) = io  $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject s')
    return $ SomeSEXP s'
  where
    s' = R.unsafeRelease s
