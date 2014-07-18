-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides helper functions and structures for safe memory
-- management when communicating with R.
--
-- 'RVal' is a wrapper around 'SEXP' preventing the 'SEXP' from being garbage
-- collected by R until the Haskell garbage collector signals that it is safe to
-- do so.

{-# LANGUAGE Rank2Types #-}
module Language.R.GC
   ( module Foreign.R.GC
   , newRVal
   , withRVal
   ) where

import Control.Monad.R
import Control.Monad.R.Unsafe (R(..))
import Foreign.R
import           Foreign.R.GC (RVal, AsSEXP)
import qualified Foreign.R.GC as Unsafe

import Control.Monad.Reader

-- | Create a new 'RVal' inside 'Control.Monad.R' monad.
--
newRVal :: AsSEXP a b => a -> R s (RVal b)
newRVal = R . ReaderT . const . Unsafe.newRVal . Unsafe.asSEXP

-- | Work with 'RVal', this function creates a temporary region, so
-- a value can escape the scope of this region, also this function
-- provides a withness variable that can be used to lift values from
-- the parent region.
withRVal :: (Unprotect b) => RVal a -> (forall s . SubRegion r s -> SEXP s a -> R s b) -> R r (UnprotectElt b)
withRVal rv body = R $ do
  envOuter <- ask
  let witness (R m) = R $ ReaderT $ \_ -> runReaderT m envOuter
  ReaderT $ const $ 
    Unsafe.withRVal rv $ \x -> runR $ body (SubRegion witness) (SEXP x)
