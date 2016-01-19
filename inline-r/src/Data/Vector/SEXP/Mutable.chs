-- |
-- Copyright: (C) 2013 Amgen, Inc.
--                2016 Tweag I/O Limited.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are wrappers over SEXP vectors used by R. Memory for vectors is
-- allocated from the R heap, and in such way that they can be converted to
-- a 'SEXP' by simple pointer arithmetic (see 'toSEXP').
--
-- Like "Data.Vector.Storable.Mutable" vectors, the vector type in this module
-- adds one extra level of indirection and a small amount of storage overhead
-- for maintainging lengths and slice offsets. If you're keeping a very large
-- number of tiny vectors in memory, you're better off keeping them as 'SEXP's
-- and calling 'fromSEXP' on-the-fly.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP.Mutable
  ( -- * Mutable slices of 'SEXP' vector types
    MVector
  , fromSEXP
  , toSEXP
  , release
  , unsafeRelease
    -- * Accessors
    -- ** Length information
  , length
  , null
    -- * Construction
    -- ** Initialisation
  , new
  , unsafeNew
  , replicate
  , replicateM
  , clone
    -- ** Extracting subvectors
  , slice
  , init
  , tail
  , take
  , drop
  , splitAt
  , unsafeSlice
  , unsafeInit
  , unsafeTail
  , unsafeTake
  , unsafeDrop
    -- ** Overlapping
  , overlaps
    -- ** Restricting memory usage
  , clear
    -- * Accessing individual elements
  , read
  , write
  , swap
  , unsafeRead
  , unsafeWrite
  , unsafeSwap
    -- * Modifying vectors
    -- ** Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
  , unsafeMove
  ) where

import Control.Monad.R.Class
import Control.Monad.R.Internal
import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable.Internal
import qualified Foreign.R as R
import Foreign.R (SEXP)
import Internal.Error

import qualified Data.Vector.Generic.Mutable as G

import Control.Applicative
import Control.Arrow ((>>>), (***))
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..), reify)
import Foreign.C
import Foreign.Storable (peekByteOff)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding
  ( length, drop, init, null, read, replicate, splitAt, tail, take )

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

-- Internal helpers
-- ----------------

phony
  :: forall s ty a b.
     (VECTOR s ty a)
  => (forall t. Reifies t (AcquireIO s) => W t ty s a -> b)
  -> MVector s ty a
  -> b
phony f v =
    reify (AcquireIO acquireIO) $ \(Proxy :: Proxy t) -> do
      f (W v :: W t ty s a)
  where
    acquireIO = violation "phony" "phony acquire called."

phony2
  :: forall s ty a b.
     (VECTOR s ty a)
  => (forall t. Reifies t (AcquireIO s) => W t ty s a -> W t ty s a -> b)
  -> MVector s ty a
  -> MVector s ty a
  -> b
phony2 f v1 v2 =
    reify (AcquireIO acquireIO) $ \(Proxy :: Proxy t) -> do
      f (W $ v1 :: W t ty s a)
        (W $ v2 :: W t ty s a)
  where
    acquireIO = violation "phony2" "phony acquire called."

-- Conversions
-- -----------

-- | /O(1)/ Create a vector from a 'SEXP'.
fromSEXP :: VECTOR s ty a => SEXP s ty -> MVector s ty a
fromSEXP sx =
    MVector sx 0 $ unsafePerformIO $ do
      fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp sx)

-- | /O(1)/ in the common case, /O(n)/ for proper slices. Convert a mutable
-- vector to a 'SEXP'. This can be done efficiently, without copy, because
-- vectors in this module always include a 'SEXP' header immediately before the
-- vector data in memory.
toSEXP
  :: (MonadR m, VECTOR (Region m) ty a)
  => MVector (Region m) ty a
  -> m (SEXP (Region m) ty)
toSEXP (MVector sx 0 len)
  | len == sexplen = return sx
  where
    sexplen = unsafePerformIO $ do
      fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp sx)
toSEXP v = toSEXP =<< clone v -- yield a zero based slice.

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: VECTOR s ty a => MVector s ty a -> Int
{-# INLINE length #-}
length = phony G.length

-- | Check whether the vector is empty.
null :: VECTOR s ty a => MVector s ty a -> Bool
{-# INLINE null #-}
null = phony G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: VECTOR s ty a => Int -> Int -> MVector s ty a -> MVector s ty a
{-# INLINE slice #-}
slice i j = phony (unW . G.slice i j)

take :: VECTOR s ty a => Int -> MVector s ty a -> MVector s ty a
{-# INLINE take #-}
take n = phony (unW . G.take n)

drop :: VECTOR s ty a => Int -> MVector s ty a -> MVector s ty a
{-# INLINE drop #-}
drop n = phony (unW . G.drop n)

splitAt :: VECTOR s ty a => Int -> MVector s ty a -> (MVector s ty a, MVector s ty a)
{-# INLINE splitAt #-}
splitAt n = phony (G.splitAt n >>> unW *** unW)

init :: VECTOR s ty a => MVector s ty a -> MVector s ty a
{-# INLINE init #-}
init = phony (unW . G.init)

tail :: VECTOR s ty a => MVector s ty a -> MVector s ty a
{-# INLINE tail #-}
tail = phony (unW . G.tail)

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: VECTOR s ty a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s ty a
            -> MVector s ty a
{-# INLINE unsafeSlice #-}
unsafeSlice i j = phony (unW . G.unsafeSlice i j)

unsafeTake :: VECTOR s ty a => Int -> MVector s ty a -> MVector s ty a
{-# INLINE unsafeTake #-}
unsafeTake n = phony (unW . G.unsafeTake n)

unsafeDrop :: VECTOR s ty a => Int -> MVector s ty a -> MVector s ty a
{-# INLINE unsafeDrop #-}
unsafeDrop n = phony (unW . G.unsafeDrop n)

unsafeInit :: VECTOR s ty a => MVector s ty a -> MVector s ty a
{-# INLINE unsafeInit #-}
unsafeInit = phony (unW . G.unsafeInit)

unsafeTail :: VECTOR s ty a => MVector s ty a -> MVector s ty a
{-# INLINE unsafeTail #-}
unsafeTail = phony (unW . G.unsafeTail)

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: VECTOR s ty a => MVector s ty a -> MVector s ty a -> Bool
{-# INLINE overlaps #-}
overlaps = phony2 G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: forall m ty a.
       (MonadR m, VECTOR (Region m) ty a)
    => Int
    -> m (MVector (Region m) ty a)
{-# INLINE new #-}
new n = withAcquire $ proxyW $ G.new n

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (MonadR m, VECTOR (Region m) ty a) => Int -> m (MVector (Region m) ty a)
{-# INLINE unsafeNew #-}
unsafeNew n = withAcquire $ proxyW $ G.unsafeNew n

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (MonadR m, VECTOR (Region m) ty a) => Int -> a -> m (MVector (Region m) ty a)
{-# INLINE replicate #-}
replicate n x = withAcquire $ proxyW $ G.replicate n x

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (MonadR m, VECTOR (Region m) ty a) => Int -> m a -> m (MVector (Region m) ty a)
{-# INLINE replicateM #-}
replicateM n m = withAcquire $ proxyW $ G.replicateM n m

-- | Create a copy of a mutable vector.
clone :: (MonadR m, VECTOR (Region m) ty a)
      => MVector (Region m) ty a
      -> m (MVector (Region m) ty a)
{-# INLINE clone #-}
clone v = withAcquire $ proxyW $ G.clone (W v)

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (MonadR m, VECTOR (Region m) ty a) => MVector (Region m) ty a -> m ()
{-# INLINE clear #-}
clear v = withAcquire $ \p -> G.clear (withW p v)

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a -> Int -> m a
{-# INLINE read #-}
read v i = withAcquire $ \p -> G.read (withW p v) i

-- | Replace the element at the given position.
write :: (MonadR m, VECTOR (Region m) ty a)
      => MVector (Region m) ty a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = withAcquire $ \p -> G.write (withW p v) i x

-- | Swap the elements at the given positions.
swap :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap v i j = withAcquire $ \p -> G.swap (withW p v) i j

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v i = withAcquire $ \p -> G.unsafeRead (withW p v) i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (MonadR m, VECTOR (Region m) ty a)
            => MVector (Region m) ty a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v i x = withAcquire $ \p -> G.unsafeWrite (withW p v) i x

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap v i j = withAcquire $ \p -> G.unsafeSwap (withW p v) i j

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (MonadR m, VECTOR (Region m) ty a) => MVector (Region m) ty a -> a -> m ()
{-# INLINE set #-}
set v x = withAcquire $ \p -> G.set (withW p v) x

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a
     -> MVector (Region m) ty a
     -> m ()
{-# INLINE copy #-}
copy v1 v2 = withAcquire $ \p -> G.copy (withW p v1) (withW p v2)

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a   -- ^ target
           -> MVector (Region m) ty a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy v1 v2 = withAcquire $ \p -> G.unsafeCopy (withW p v1) (withW p v2)

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a
     -> MVector (Region m) ty a
     -> m ()
{-# INLINE move #-}
move v1 v2 = withAcquire $ \p -> G.move (withW p v1) (withW p v2)

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a             -- ^ target
           -> MVector (Region m) ty a             -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove v1 v2 = withAcquire $ \p -> G.unsafeMove (withW p v1) (withW p v2)
