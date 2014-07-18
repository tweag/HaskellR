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
-- Implementation notes:
--  
--   * The current solution does not support nested regions, so passing a value to the
--     child region from a parent is not possible even it's safe.
--
-- Protection of values:
--
-- Any value can be protected by using 'protect' function from the 'Protect' typeclass, this 
-- function guarantees that value is protected withing a region and can be used there.
--
-- Once a value is leaving region, either by a final statement or by passing into any communication
-- channel it should be unprotected, so to be prepared to be used outside a region. This involve
-- 2 things:
--
--    1. Value will change type to the type that doesn't contain a reference to a region;
--
--    2. Value will be marked as 'UnsafeValue' if it's not safe to use without a protection.
--    (see 'UnsafeValue' for complete documentation)
--    
--    3. Value is fully evaluated, so it doesn't contain any thunks to a variables that are
--    safe to use only inside a region.
--
-- It's still safe to use original value inside a region even after 'unprotection' procedure.
-- 'runRegion' procedure takes care of unprotection procedure, however user should do it himself
-- before passing a value into the communication channel.
--
{-# LANGUAGE Rank2Types #-}
module Control.Monad.R
  ( -- * The R monad
    R
  , io
  , MonadR
  , runR
    -- * Regions
    -- ** Execution
  , runRegion
  , protectRegion
  , unsafeRunRegion
  , SubRegion(..)
  , newRegion
    -- * Protection
  , Protect(..)
  , Unprotect(..)
  , UnsafeValue
  , unsafeUseValue
    -- * Operations lifting
  , Foreign.R.liftProtect 
  ) where

import Control.Monad.R.Unsafe
import Control.Monad.R.Class
import Foreign.R

import Control.Monad.Reader
import Data.IORef

-- This guarantee safeness of values usafe inside a region.
-- 
-- 'Unprotect' constraint allow to run unprotect procedure on the values that 
-- leaving a channel.
runRegion :: (Unprotect a) => (forall s. R s a) -> R s' (UnprotectElt a)
runRegion = R . ReaderT . const . unsafeRunRegion


-- | Run R region inside an IO monad.
--
-- This method will not allow to to return a values that have an @s@ variable
unsafeRunRegion :: (Unprotect a) => (forall s . R s a) -> IO (UnprotectElt a)
unsafeRunRegion f = internalRunRegion (unprotect =<< f)

unsafeRunRegionWith :: (Unprotect a) => (forall s .R s a) -> IORef Int -> IO (UnprotectElt a)
unsafeRunRegionWith f x = internalRunRegionWith (unprotect =<< f) x 

-- | A witness that the region 'r' is older than
-- (or, is the parent of, the subtype of) the region labeled 's'
newtype SubRegion r s = SubRegion (forall v . R r v -> R s v)

-- | A function that allow to share variables from the partent region in the current
-- one. This apporach slightly differs from the one that was originaly described in
-- papar by Oleg Kiselov.
--
-- The main reason it that R protection stack doesn't allow to register values inside
-- a parent region, so every value even tagged as registered in a parent region will 
-- be unprotected when it leaves the scope of the function. However this is safe as
-- 'Unprotect' constraint cleares information about region where value was protected.
-- 
-- Usage:
--
-- > newRegion $ (SubRegion witness) -> do
-- >   z <-witness $ getAttribute (x {- value from the parent region -})
-- >   use z
--
newRegion :: (Unprotect v) => (forall s . SubRegion r s -> R s v) -> R r (UnprotectElt v)
newRegion body = R $ do
   -- Create a counter for a new region
   t <- ReaderT $ const $ newIORef 0
   -- Create a 'witness' function that changes the label of the region. this function
   -- unsafely runs a function from the parent region with _current_ counter. The
   -- result will be tagged with the current region
   let witness (R m) = unsafeIOToR $ runReaderT m t 
   ReaderT $ const $ unsafeRunRegionWith (body (SubRegion witness)) t
