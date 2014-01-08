-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are a special instance of "Data.Vector.Storable.Mutable" where the
-- memory is allocated from the R heap, and in such a way that they can be
-- converted to a 'SEXP' through simple pointer arithmetic (see 'toSEXP').

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP.Mutable
  ( MVector(..)
  , fromSEXP
  , toSEXP
  , module Data.Vector.Generic.Mutable
  ) where

import H.Internal.Prelude
import Data.Vector.SEXP.Base
import qualified Foreign.R as R
import Foreign.R.Type (SSEXPTYPE, IsVector)

import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
import qualified Data.Vector.Storable.Mutable as Vector
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Generic.Mutable hiding (MVector(..))
import Data.Singletons (SingI, fromSing, sing)

import Control.Applicative
import Foreign (Ptr)
import Foreign.Storable
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr_)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

-- | Mutable R vector. They are represented in memory with the same header as
-- 'SEXP' nodes. The second type paramater is a phantom parameter reflecting at
-- the type level the tag of the vector when viewed as a 'SEXP'.
newtype MVector (ty :: SEXPTYPE) s a = MVector { unMVector :: Vector.MVector s a }

instance (IsVector ty, SingI ty, Storable a, a ~ ElemRep ty)
         => G.MVector (MVector ty) a where
  basicLength             = G.basicLength . unMVector
  basicUnsafeSlice j m mv = MVector $ G.basicUnsafeSlice j m (unMVector mv)
  basicOverlaps mv1 mv2   = G.basicOverlaps (unMVector mv1) (unMVector mv2)
  basicUnsafeNew n
    -- R calls using allocVector() for CHARSXP "defunct"...
    | fromSing (sing :: SSEXPTYPE ty) == R.Char =
      failure "Data.Vector.SEXP.Mutable.new"
              "R character vectors are immutable and globally cached. Use 'mkChar' instead."
    | otherwise =
      fromSEXP =<< unsafePrimToPrim (R.allocVector (sing :: SSEXPTYPE ty) n)
  basicUnsafeRead mv i    = G.basicUnsafeRead (unMVector mv) i
  basicUnsafeWrite mv i x = G.basicUnsafeWrite (unMVector mv) i x
  basicSet mv x           = G.basicSet (unMVector mv) x
  basicUnsafeCopy mv1 mv2 = G.basicUnsafeCopy (unMVector mv1) (unMVector mv2)
  basicUnsafeMove mv1 mv2 = G.basicUnsafeMove (unMVector mv1) (unMVector mv2)

-- | /O(1)/ Create a vector from a 'SEXP'.
fromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
         => R.SEXP ty
         -> m (MVector ty (PrimState m) a)
fromSEXP s = unsafePrimToPrim $ do
    len  <- R.length s
    fptr <- castForeignPtr <$> newForeignPtr_ (R.unsafeSEXPToVectorPtr s)
    return $
      MVector $ Vector.unsafeFromForeignPtr0 fptr (fromIntegral len)

-- | /O(1)/ Convert a mutable vector to a 'SEXP'. This can be done efficiently,
-- without copy, because vectors in this module always include a 'SEXP' header
-- immediately before the vector data in memory.
toSEXP :: forall a s ty. (E ty a, IsVector ty, Storable a)
       => MVector ty s a
       -> R.SEXP ty
toSEXP = (`R.unSomeSEXP` R.unsafeCoerce)
       . (R.unsafeVectorPtrToSEXP :: Ptr a -> R.SomeSEXP)
       . unsafeForeignPtrToPtr
       . fst
       . Vector.unsafeToForeignPtr0
       . unMVector
