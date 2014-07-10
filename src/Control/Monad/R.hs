-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Monad R provides a safe way to work with R object, this monad performs all required
-- variable protection under the hood. The approach to the regions is a simplified
-- lightweigh regions.
--
-- Inside R monad it's possible to protect 'Foreign.R.Internal.SEXP' by adding them into R protection stack
-- and once block is finished all values will be unprotected in one call. This basically
-- doesn't mean that values will be freed by R runtime, but it will not be safe to access
-- them. R monad also solves a problem of 'leaking' resources, so no value that is protected
-- withing a block can leave block scope.
--
-- All values that should be protected in a scope of 'R' monad should contain @s@ type variable
-- see 'Foreign.R' for an example.
--
-- It's possible to run a protection block, all resources that are allocated withing a
-- block are protected by calling 'protect' and once block is finished, then all 
--
-- > runRegion $ do
-- >   x <- fromSEXP <$> protect foo
-- >   y <- fromSEXP <$> protect bar
-- >   foo (x,y)
--
-- Here @x@ and @y@ will be protected during the computation of @foo@ and will be unprotected
-- just before a return.
-- 
-- Implementation notes:'
--  
--   * The current solution does not support nested regions, so passing a value to the
--     child region from a parent is not possible even it's safe.
--
{-# LANGUAGE Rank2Types #-}
module Control.Monad.R
  ( -- * The R monad
    R
  , io
  , MonadR
    -- * Regions
    -- ** Execution
  , runR
  , runRegion
  , protectRegion
  , unsafeRunRegion
    -- * Protection
  , ProtectElt
  , UnprotectElt
  , Protect(..)
  , Unprotect(..)
  , UnsafeValue
  , unsafeUseValue
  , mkUnsafe
    -- * Operations lifting
  , Foreign.R.liftProtect 
  ) where

import Control.Monad.R.Unsafe
import Control.Monad.R.Class
import Foreign.R

import Control.Monad.Reader

-- | Run a R code in a region. See $regions
--
-- This function creates a nested region and makes all objects inside a regions
-- as free.
--
-- 'NFData' constraint is used to prevent from leaks of any unevaluated objects
-- through the pure values.
runRegion :: (Unprotect a) => (forall s. R s a) -> R s' (UnprotectElt a)
runRegion = R . ReaderT . const . unsafeRunRegion

unsafeRunRegion :: (Unprotect a) => (forall s . R s a) -> IO (UnprotectElt a)
unsafeRunRegion f = unsafeRunRegion_ (unprotect =<< f)
