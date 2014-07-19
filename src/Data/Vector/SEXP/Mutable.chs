-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are wrappers over SEXP vectors used by R. Memory for vectors is
-- allocated from the R heap, and in such way that they can be converted to
-- a 'SEXP' through a simple pointer arithmetics (see 'toSEXP').
--
-- 'SEXP' Vectors are not protected by the Haskell, so all protection rules
-- for 'SEXP' are applied to 'SEXP' vectors, so one may need to protect and
-- unprotect values manually.
--
-- SEXP header is allocated before a data, so there is no additional pointer
-- jump to reach the data, but the tradeoff is that all slicing operations
-- are banned. If you need to use slicing operations you may convert vector
-- to Storable, see 'toStorable' and 'unsafeToStorable'.


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP.Mutable
  (
    -- * Mutable vectors of 'SEXP' types
    MVector(..), IOVector, STVector
  -- * Accessors
  -- ** Length information
  , length, null
  -- * Construction
  -- ** Initialisation
  , new, unsafeNew, replicate, replicateM, clone
  -- ** Restricting memory usage
  , clear
  -- * Accessing individual elements
  , read, write, swap
  , unsafeRead, unsafeWrite, unsafeSwap
  -- * Modifying vectors
  -- ** Filling and copying
  , set, copy, move, unsafeCopy, unsafeMove

  -- * SEXP specific.
  , fromSEXP
  , toSEXP
  , unsafeToStorable
  , fromStorable
  ) where

import H.Internal.Prelude
import Data.Vector.SEXP.Base
import qualified Foreign.R as R
import Foreign.R.Type (SSEXPTYPE, IsVector)

import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld, unsafePrimToPrim, unsafeInlineIO)
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Storable.Mutable as Storable
import Data.Singletons (SingI, fromSing, sing)

import Foreign (castPtr, Ptr, withForeignPtr, plusPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array ( copyArray, moveArray )

import Prelude hiding ( length, null, replicate, read  )

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

-- | Mutable R vector. They are represented in memory with the same header as
-- 'SEXP' nodes. The second type paramater is a phantom parameter reflecting at
-- the type level the tag of the vector when viewed as a 'SEXP'.
newtype MVector (ty :: SEXPTYPE) s a = MVector { unMVector :: SEXP ty }

type IOVector ty   = MVector ty RealWorld
type STVector ty s = MVector ty s

type SexpVector ty a = (Storable a, IsVector ty, SingI ty, ElemRep ty ~ a)

instance (IsVector ty, SingI ty, Storable a, a ~ ElemRep ty)
         => G.MVector (MVector ty) a where
  basicLength (MVector s) = unsafeInlineIO $
    fmap fromIntegral ({# get VECSEXP->vecsxp.length #} s)
  basicUnsafeSlice j m v
    | j == 0 && m == G.basicLength v = v
    | j == 0 = unsafeInlineIO $ do
--      FIXME temporary hack to workaround this change will break evething
        poke ((castPtr $ unMVector v :: Ptr ()) `plusPtr` 24) (fromIntegral m :: CInt)
--        {# set VECSEXP->vecsxp.length #} (unMVector v) (fromIntegral m :: CInt)
        return v
    | otherwise = error "unsafeSlice is not supported for SEXP vectors, to perform slicing convert vector to Storable"
  basicOverlaps mv1 mv2   = unMVector mv1 == unMVector mv2
  basicUnsafeNew n
    -- R calls using allocVector() for CHARSXP "defunct"...
    | fromSing (sing :: SSEXPTYPE ty) == R.Char =
      failure "Data.Vector.SEXP.Mutable.new"
              "R character vectors are immutable and globally cached. Use 'mkChar' instead."
    | otherwise =
      fromSEXP =<< unsafePrimToPrim (R.allocVector (sing :: SSEXPTYPE ty) n)
  basicUnsafeRead mv i     = unsafePrimToPrim
                           $ peekElemOff (toVecPtr mv) i
  basicUnsafeWrite mv i x  = unsafePrimToPrim
                           $ pokeElemOff (toVecPtr mv) i x
  basicSet mv x            = Prelude.mapM_ (\i -> G.basicUnsafeWrite mv i x) [0..G.basicLength mv]
  basicUnsafeCopy mv1 mv2  = unsafePrimToPrim $ do
      copyArray (toVecPtr mv1)
                (toVecPtr mv2)
                (G.basicLength mv1)
  basicUnsafeMove mv1 mv2  = unsafePrimToPrim $ do
      moveArray (toVecPtr mv1)
                (toVecPtr mv2)
                (G.basicLength mv1)

toVecPtr :: MVector ty s a -> Ptr a
toVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unMVector mv)

-- | /O(1)/ Create a vector from a 'SEXP'.
fromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
         => R.SEXP ty
         -> m (MVector ty (PrimState m) a)
fromSEXP s = return (MVector s)

-- | /O(1)/ Convert a mutable vector to a 'SEXP'. This can be done efficiently,
-- without copy, because vectors in this module always include a 'SEXP' header
-- immediately before the vector data in memory.
toSEXP :: forall a s ty. (E ty a, IsVector ty, Storable a)
       => MVector ty s a
       -> R.SEXP ty
toSEXP = unMVector

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: SexpVector ty a => MVector ty s a -> Int
{-# INLINE length #-}
length (MVector s) = unsafeInlineIO $
    fmap fromIntegral ({# get VECSEXP->vecsxp.length #} s)

-- | Check whether the vector is empty
null :: SexpVector ty a => (MVector ty) s a -> Bool
{-# INLINE null #-}
null (MVector s) = unsafeInlineIO $
    fmap ((/= (0::Int)) . fromIntegral) ({# get VECSEXP->vecsxp.length #} s)

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, SexpVector ty a) => Int -> m (MVector ty (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, SexpVector ty a) => Int -> m (MVector ty (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, SexpVector ty a) => Int -> a -> m (MVector ty (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, SexpVector ty a) => Int -> m a -> m (MVector ty (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, SexpVector ty a)
      => MVector ty (PrimState m) a -> m (MVector ty (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, SexpVector ty a) => MVector ty (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, SexpVector ty a)
     => MVector ty (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, SexpVector ty a)
      => MVector ty (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, SexpVector ty a)
     => MVector ty (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, SexpVector ty a)
           => MVector ty (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, SexpVector ty a)
            => MVector ty (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, SexpVector ty a)
           => MVector ty (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, SexpVector ty a) => MVector ty (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, SexpVector ty a)
     => MVector ty (PrimState m) a
     -> MVector ty (PrimState m) a
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, SexpVector ty a)
           => MVector ty (PrimState m) a   -- ^ target
           -> MVector ty (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, SexpVector ty a)
     => MVector ty (PrimState m) a
     -> MVector ty (PrimState m) a
     -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, SexpVector ty a)
           => MVector ty (PrimState m) a          -- ^ target
           -> MVector ty (PrimState m) a          -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | O(1) Inplace convertion to Storable vector.
unsafeToStorable :: (PrimMonad m, SexpVector ty a)
                 => MVector ty (PrimState m) a           -- ^ target
                 -> m (Storable.MVector (PrimState m) a) -- ^ source
{-# INLINE unsafeToStorable #-}
unsafeToStorable v@(MVector p) = unsafePrimToPrim $ do
  R.preserveObject p
  post <- getPostToCurrentRThread
  ptr <- newForeignPtr (toVecPtr v) (post $ R.releaseObject (castPtr $ toVecPtr v))
  return $ Storable.unsafeFromForeignPtr0 ptr (length v)

-- | O(N) Convertion from storable vector to SEXP vector.
fromStorable :: (PrimMonad m, SexpVector ty a)
             => Storable.MVector (PrimState m) a
             -> m (MVector ty (PrimState m) a)
{-# INLINE fromStorable #-}
fromStorable v = do
  let (fptr, l) = Storable.unsafeToForeignPtr0 v
  mv <- new l
  unsafePrimToPrim $ withForeignPtr fptr $ \p -> do
    copyArray p (toVecPtr mv) (Storable.length v)
  return mv
