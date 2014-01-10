-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are a special instance of "Data.Vector.Storable" where the memory is
-- allocated from the R heap, and in such a way that they can be converted to
-- a 'SEXP' through simple pointer arithmetic (see 'toSEXP').

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP
  ( Vector(..)
  , MVector(..)
  , ElemRep
  , Data.Vector.SEXP.fromSEXP
  , unsafeFromSEXP
  , Data.Vector.SEXP.toSEXP
  , unsafeToSEXP
  , module Data.Vector.Generic
  , toString
  , toByteString
  ) where

import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable as Mutable hiding ( length )
import Foreign.R ( SEXP )
import Foreign.R.Type ( SEXPTYPE(Char), IsVector )

import Control.Monad.Primitive ( PrimMonad )
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Generic as G
import Data.Vector.Generic hiding ( Vector )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as B
import Data.Singletons (SingI)

import Control.Arrow ( first )
import Control.Monad ( liftM )
import Data.Word ( Word8 )
import Foreign ( castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Prelude hiding ( length, head )

newtype Vector (ty :: SEXPTYPE) a = Vector { unVector :: Vector.Vector a }
    deriving (Eq)

type instance G.Mutable (Vector ty) = MVector ty

instance (IsVector ty, Storable a, SingI ty, a ~ ElemRep ty)
         => G.Vector (Vector ty) a where
  basicUnsafeFreeze mv  = liftM Vector  $ G.basicUnsafeFreeze $ unMVector mv
  basicUnsafeThaw v     = liftM MVector $ G.basicUnsafeThaw $ unVector v
  basicLength           = G.basicLength . unVector
  basicUnsafeSlice i n  = Vector . G.basicUnsafeSlice i n . unVector
  basicUnsafeIndexM v i = G.basicUnsafeIndexM (unVector v) i
  basicUnsafeCopy mv v  = G.basicUnsafeCopy (unMVector mv) (unVector v)
  elemseq v             = G.elemseq (unVector v)

-- | /O(n)/ Create an immutable vector from a 'SEXP'. Because 'SEXP's are
-- mutable, this function yields an immutable copy of the 'SEXP'.
fromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
         => SEXP ty
         -> m (Vector ty a)
fromSEXP s = G.freeze =<< Mutable.fromSEXP s

-- | /O(1)/ Unsafe convert a mutable 'SEXP' to an immutable vector without
-- copying. The mutable vector must not be used after this operation, lest one
-- runs the risk of breaking referential transparency.
unsafeFromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
               => SEXP ty
               -> m (Vector ty a)
unsafeFromSEXP s = G.unsafeFreeze =<< Mutable.fromSEXP s

-- | /O(n)/ Yield a (mutable) copy of the vector as a 'SEXP'.
toSEXP :: (E ty a, IsVector ty, Storable a, PrimMonad m)
       => Vector ty a
       -> m (SEXP ty)
toSEXP = liftM Mutable.toSEXP . G.thaw

-- | /O(1)/ Unsafely convert an immutable vector to a (mutable) 'SEXP' without
-- copying. The immutable vector must not be used after this operation.
unsafeToSEXP :: (E ty a, IsVector ty, Storable a, PrimMonad m)
             => Vector ty a
             -> m (SEXP ty)
unsafeToSEXP = liftM Mutable.toSEXP . G.unsafeThaw

-- | /O(n)/ Convert a character vector into a 'String'.
toString :: Vector 'Char Word8 -> String
toString v = unsafePerformIO $ peekCString . castPtr
         . unsafeForeignPtrToPtr . fst
         . Vector.unsafeToForeignPtr0
         . unVector $ v

-- | /O(1)/ Convert a character vector into a strict 'ByteString'.
toByteString :: Vector 'Char Word8 -> ByteString
toByteString = unsafePerformIO
             . B.unsafePackCStringLen
             . first castPtr
             . first unsafeForeignPtrToPtr
             . Vector.unsafeToForeignPtr0
             . unVector
