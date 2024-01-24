-- |
-- Copyright: (C) 2016 Tweag I/O Limited.

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--ple" @-}

module Data.Vector.SEXP.Mutable.Internal
  ( MVector(..)
  , W(..)
  , withW
  , mvtypeOf
  , proxyW
  , unsafeToPtr
  , release
  , unsafeRelease
  ) where

import Control.Memory.Region
import qualified Foreign.R as R

import Control.Monad.Primitive (unsafePrimToPrim)
import Control.Monad.R.Internal
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..))
import qualified Data.Vector.Generic.Mutable as G
import Foreign (Storable(..), Ptr, castPtr)
import Foreign.C   -- Needed to help LH name resolution
import Foreign.Marshal.Array (advancePtr, copyArray, moveArray)
import Foreign.R (SEXP)
import Internal.Error

{-@ measure Data.Vector.SEXP.Mutable.Internal.mvtypeOf :: MVector s a -> SEXPTYPE @-}
{-@ type TMVector s a T = {v:MVector s a | Data.Vector.SEXP.Mutable.Internal.mvtypeOf v == T} @-}

-- | Mutable R vector. Represented in memory with the same header as 'SEXP'
-- nodes. The second type parameter is phantom, reflecting at the type level the
-- tag of the vector when viewed as a 'SEXP'. The tag of the vector and the
-- representation type are related via 'ElemRep'.
data MVector s a = MVector
  { mvectorBase :: (SEXP s)
  , mvectorOffset :: Int32
  , mvectorLength :: Int32
  }
-- TODO: Declaring fields with the UNPACK pragma and strict interferes with LH

{-@ assume mvtypeOf :: v:MVector s a -> {t:R.SEXPTYPE | t == Data.Vector.SEXP.Mutable.Internal.mvtypeOf v } @-}
mvtypeOf :: MVector s a -> R.SEXPTYPE
mvtypeOf mv = R.typeOf (mvectorBase mv)

-- | Internal wrapper type for reflection. First type parameter is the reified
-- type to reflect.
newtype W t s a = W { unW :: MVector s a }

instance (Reifies t (AcquireIO s, IO R.SEXPTYPE), Storable a) => G.MVector (W t) a where
#if MIN_VERSION_vector(0,11,0)
  basicInitialize _ = return ()
#endif
  {-# INLINE basicLength #-}
  basicLength (unW -> MVector _ _ len) = fromIntegral len

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (unW -> MVector ptr off _len) =
      W $ MVector ptr (off + fromIntegral j) (fromIntegral m)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (unW -> MVector ptr1 off1 len1) (unW -> MVector ptr2 off2 len2) =
      ptr1 == ptr2 && (off2 < off1 + len1 || off1 < off2 + len2)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
      -- R calls using allocVector() for CHARSXP "defunct"...
      ty <- unsafePrimToPrim getSEXPTYPE
      case ty of
        R.SChar -> failure "Data.Vector.SEXP.Mutable.new"
                          "R character vectors are immutable and globally cached. Use 'mkChar' instead."
        _ -> do
          sx <- unsafePrimToPrim (acquireIO =<< R.allocVector ty n)
          return $ W $ MVector (R.unsafeRelease sx) 0 (fromIntegral n)
    where
      (acquireIO, getSEXPTYPE) = reflect (Proxy :: Proxy t)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (unW -> mv) i =
      unsafePrimToPrim $ peekElemOff (unsafeToPtr mv) i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (unW -> mv) i x =
      unsafePrimToPrim $ pokeElemOff (unsafeToPtr mv) i x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy w1@(unW -> mv1) (unW -> mv2) = unsafePrimToPrim $ do
      copyArray (unsafeToPtr mv1)
                (unsafeToPtr mv2)
                (G.basicLength w1)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove w1@(unW -> mv1) (unW -> mv2)  = unsafePrimToPrim $ do
      moveArray (unsafeToPtr mv1)
                (unsafeToPtr mv2)
                (G.basicLength w1)

{-@ ignore unsafeToPtr @-}
unsafeToPtr :: Storable a => MVector s a -> Ptr a
unsafeToPtr (MVector sx off _) =
    castPtr (R.unsafeSEXPToVectorPtr sx) `advancePtr` fromIntegral off

{- assume proxyW :: forall <p :: MVector s a -> Bool > . m ((W t s a)<\w -> p (unW w)>) -> proxy t -> m ((MVector s a)<p>) @-}
proxyW :: Monad m => m (W t s a) -> proxy t -> m (MVector s a)
proxyW m _ = fmap unW m

-- withW :: proxy t -> MVector s ty a -> W t ty s a
withW :: proxy t -> MVector s a -> W t s a
withW _ v = W v

{- assume release :: x:MVector s a -> TMVector s' a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
release :: SubRegion s' s => MVector s a -> MVector s' a
release = unsafeRelease

{-@ assume unsafeRelease :: x:MVector s a -> TMVector s' a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeRelease :: MVector s a -> MVector s' a
unsafeRelease (MVector b o l) = MVector (R.unsafeRelease b) o l
