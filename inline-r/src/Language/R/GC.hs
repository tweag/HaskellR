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

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
module Language.R.GC
  ( automatic
  ) where

import Foreign.C -- only needed to help name resolution in LH
import Control.Monad.Primitive -- only needed to help name resolution in LH
import Control.Memory.Region
import Control.Monad.R.Class
import Control.Exception
import qualified Foreign.R as R
import System.Mem.Weak (addFinalizer)

-- Helps LH name resolution. Otherwise ~ isn't found.
_f :: a ~ b => a -> b -> CString -> m (PrimState m)
_f = undefined

{-@ automatic :: MonadR m => a:SEXP s -> m (TSEXP G (typeOf a)) @-}
-- | Declare memory management for this value to be automatic. That is, the
-- memory associated with it may be freed as soon as the garbage collector
-- notices that it is safe to do so.
--
-- Values with automatic memory management are tagged with the global region.
-- The reason is that just like for other global values, deallocation of the
-- value can never be observed. Indeed, it is a mere "optimization" to
-- deallocate the value sooner - it would still be semantically correct to never
-- deallocate it at all.
automatic :: MonadR m => R.SEXP s -> m (R.SEXP G)
automatic s = io $ mask_ $ do
    R.preserveObject s'
    s' `addFinalizer` (R.releaseObject (R.unsafeRelease s'))
    return s'
  where
    s' = R.unsafeRelease s
