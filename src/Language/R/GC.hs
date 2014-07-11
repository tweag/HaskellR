-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE Rank2Types #-}
module Language.R.GC
   ( module Foreign.R.GC
   ) where

import Control.Monad.R
import Control.Monad.R.Unsafe
import Foreign.R
import           Foreign.R.GC (RVal, AsSEXP)
import qualified Foreign.R.GC as Unsafe

import Control.Monad.Reader

-- | Create a new 'RVal' inside 'Control.Monad.R' monad.
--
newRVal :: AsSEXP a b => a -> R s (RVal b)
newRVal = unsafeIOToR . Unsafe.newRVal . Unsafe.asSEXP

-- | Work with 'RVal', this function creates a temporary region, so
-- a value can escape the scope of this region, also this function
-- provides a withness variable that can be used to lift values from
-- the parent region.
withRVal :: (Unprotect b) => RVal a -> (forall s . SubRegion r s -> SEXP s a -> R s b) -> R r (UnprotectElt b)
withRVal rv body = R $ do
  envOuter <- ask
  let witness (R m) = unsafeIOToR $ runReaderT m envOuter
  ReaderT $ const $ 
    Unsafe.withRVal rv $ \x -> unsafeRunRegion $ body (SubRegion witness) (SEXP x)
