-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Facilities to get Haskell's garbage collector to manage the liveness of
-- values allocated on the R heap. By default, R values remain live so long as
-- the current region is extant. The R garbage collector may only free them
-- after the end of the region. Sometimes, this discipline incurs too high of
-- a memory usage and nested regions are not always a solution.
--
-- This module enables registering a callback with the GHC garbage collector. In
-- this way, when the GHC garbage collector detects that a value is no longer
-- live, we can notify the R garbage collector of this fact. The R garbage
-- collector is then free to deallocate the memory associated with the value
-- soon after that.
--
-- This module hence offers an alternative, more flexible memory management
-- discipline, at a performance cost. In particular, collections of many small,
-- short-lived objects are best managed using regions.

module Language.R.GC
  ( automatic
  , automaticSome
  ) where

import Control.Memory.Region
import H.Internal.Prelude
import qualified Foreign.R as R
import System.Mem.Weak (addFinalizer)

-- | Declare memory management for this value to be automatic. That is, the
-- memory associated with it may be freed as soon as the garbage collector
-- notices that it is safe to do so.
--
-- Values with automatic memory management are tagged with the global region.
-- The reason is that just like for other global values, deallocation of the
-- value can never be observed. Indeed, it is a mere "optimization" to
-- deallocate the value sooner - it would still be semantically correct to never
-- deallocate it at all.
automatic :: MonadR m => R.SEXP s a -> m (R.SEXP G a)
automatic s = io $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject (R.unsafeRelease s'))
    return s'
  where
    s' = R.unsafeRelease s

-- | 'automatic' for 'SomeSEXP'.
automaticSome :: MonadR m => R.SomeSEXP s -> m (R.SomeSEXP G)
automaticSome (SomeSEXP s) = io  $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject s')
    return $ SomeSEXP s'
  where
    s' = R.unsafeRelease s
