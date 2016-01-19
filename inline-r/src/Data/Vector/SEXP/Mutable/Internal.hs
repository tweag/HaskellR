-- |
-- Copyright: (C) 2016 Tweag I/O Limited.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vector.SEXP.Mutable.Internal
  ( MVector(..)
  , W(..)
  , withW
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
import Data.Singletons (fromSing, sing)
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.SEXP.Base
import Foreign (castPtr, Ptr)
import Foreign.Marshal.Array (copyArray, moveArray)
import Foreign.R (SEXP)
import Foreign.R.Type (SSEXPTYPE)
import Foreign.Storable
import Internal.Error

-- | Mutable R vector. Represented in memory with the same header as 'SEXP'
-- nodes. The second type parameter is phantom, reflecting at the type level the
-- tag of the vector when viewed as a 'SEXP'. The tag of the vector and the
-- representation type are related via 'ElemRep'.
data MVector s ty a = MVector
  { mvectorBase :: {-# UNPACK #-} !(SEXP s ty)
  , mvectorOffset :: {-# UNPACK #-} !Int32
  , mvectorLength :: {-# UNPACK #-} !Int32
  }

-- | Internal wrapper type for reflection. First type parameter is the reified
-- type to reflect.
newtype W t ty s a = W { unW :: MVector s ty a }

instance (Reifies t (AcquireIO s), VECTOR s ty a) => G.MVector (W t ty) a where
  {-# INLINE basicLength #-}
  basicLength (unW -> MVector _ _ len) = fromIntegral len

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (unW -> MVector ptr off len) =
      W $ MVector ptr (off + fromIntegral j) (fromIntegral m)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (unW -> MVector ptr1 off1 len1) (unW -> MVector ptr2 off2 len2) =
      ptr1 == ptr2 && (off2 < off1 + len1 || off1 < off2 + len2)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    -- R calls using allocVector() for CHARSXP "defunct"...
    | fromSing (sing :: SSEXPTYPE ty) == R.Char =
      failure "Data.Vector.SEXP.Mutable.new"
              "R character vectors are immutable and globally cached. Use 'mkChar' instead."
    | otherwise = do
      sx <- unsafePrimToPrim (acquireIO =<< R.allocVector (sing :: SSEXPTYPE ty) n)
      return $ W $ MVector (R.unsafeRelease sx) 0 (fromIntegral n)
    where
      AcquireIO acquireIO = reflect (Proxy :: Proxy t)

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

unsafeToPtr :: MVector s ty a -> Ptr a
unsafeToPtr v = castPtr (R.unsafeSEXPToVectorPtr $ mvectorBase v)

proxyW :: Monad m => m (W t ty s a) -> proxy t -> m (MVector s ty a)
proxyW m _ = fmap unW m

withW :: proxy t -> MVector s ty a -> W t ty s a
withW _ v = W v

release :: (g <= s) => MVector s ty a -> MVector g ty a
release = unsafeRelease

unsafeRelease :: MVector s ty a -> MVector g ty a
unsafeRelease (MVector b o l) = MVector (R.unsafeRelease b) o l
