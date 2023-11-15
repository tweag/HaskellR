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

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--max-case-expand=0" @-}
module Data.Vector.SEXP.Mutable
  ( -- * Mutable slices of 'SEXP' vector types
    MVector
  , fromSEXP
  , toSEXP
  , release
  , unsafeRelease
  , isVectorType
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

import Control.Exception (evaluate)
import qualified Control.Memory.Region  -- XXX: needed for LH name resolution
import qualified Foreign.C              -- XXX: needed for LH name resolution
import Foreign.R.Type         -- XXX: needed for LH name resolution
import Control.Monad.R.Class
import Control.Monad.R.Internal
import Data.Vector.SEXP.Mutable.Internal
import qualified Foreign.R as R
import Foreign.R (SEXP)
import Foreign.Storable (Storable)
import Internal.Error

import qualified Data.Vector.Generic.Mutable as G

import Control.Applicative
import Control.Arrow ((>>>), (***))
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..), reify)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding
  ( length, drop, init, null, read, replicate, splitAt, tail, take )

-- Internal helpers
-- ----------------

phony
  :: forall s a b.
     (forall t. Reifies t (AcquireIO s, IO R.SEXPTYPE) => W t s a -> b)
  -> MVector s a
  -> b
phony f v =
    reify (acquireIO) $ \(Proxy :: Proxy t) -> do
      f (W v :: W t s a)
  where
    acquireIO = violation "phony" "phony acquire or SEXPTYPE called."

phony2
  :: forall s a b.
     (forall t. Reifies t (AcquireIO s, IO R.SEXPTYPE) => W t s a -> W t s a -> b)
  -> MVector s a
  -> MVector s a
  -> b
phony2 f v1 v2 =
    reify acquireIO $ \(Proxy :: Proxy t) -> do
      f (W $ v1 :: W t s a)
        (W $ v2 :: W t s a)
  where
    acquireIO = violation "phony2" "phony acquire or SEXPTYPE called."

{-@ reflect isVectorType @-}
isVectorType :: R.SEXPTYPE -> Bool
isVectorType t = case t of
    R.SChar -> True
    R.Logical -> True
    R.SInt -> True
    R.Real -> True
    R.SComplex -> True
    R.SString -> True
    R.SVector -> True
    R.Expr -> True
    R.WeakRef -> True
    R.Raw -> True
    _ -> False

-- Conversions
-- -----------

-- | /O(1)/ Create a vector from a 'SEXP'.
{-@ assume fromSEXP :: {x:SEXP s | isVectorType (typeOf x) } -> TMVector s a (typeOf x) @-}
{-@ ignore fromSEXP @-}
fromSEXP :: SEXP s -> MVector s a
fromSEXP sx =
    MVector sx 0 $ unsafePerformIO $ do
      fromIntegral <$> R.length sx

-- | /O(1)/ in the common case, /O(n)/ for proper slices. Convert a mutable
-- vector to a 'SEXP'. This can be done efficiently, without copy, because
-- vectors in this module always include a 'SEXP' header immediately before the
-- vector data in memory.
{-@ assume toSEXP :: v:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a -> m (TSEXP (Region m) (Data.Vector.SEXP.Mutable.Internal.mvtypeOf v)) @-}
{-@ ignore toSEXP @-}
toSEXP
  :: (MonadR m, Storable a)
  => MVector (Region m) a
  -> m (SEXP (Region m))
toSEXP (MVector sx 0 len)
  | len == sexplen = return sx
  where
    sexplen = unsafePerformIO $ do
      fromIntegral <$> R.length sx
toSEXP v = toSEXP =<< clone v -- yield a zero based slice.

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Storable a => MVector s a -> Int
{-# INLINE length #-}
length = phony G.length

-- | Check whether the vector is empty.
null :: Storable a => MVector s a -> Bool
{-# INLINE null #-}
null = phony G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
{-@ assume slice :: Int -> Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
slice :: Storable a => Int -> Int -> MVector s a -> MVector s a
{-# INLINE slice #-}
slice i j = phony (unW . G.slice i j)

{-@ assume take :: Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
take :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take n = phony (unW . G.take n)

{-@ assume drop :: Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
drop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop n = phony (unW . G.drop n)

{-@ assume splitAt :: Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> (TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x), TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)) @-}
{-@ ignore splitAt @-}
splitAt :: Storable a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt n = phony (G.splitAt n >>> unW *** unW)

{-@ assume init :: x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
init :: Storable a => MVector s a -> MVector s a
{-# INLINE init #-}
init = phony (unW . G.init)

{-@ assume tail :: x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
tail :: Storable a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = phony (unW . G.tail)

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
{-@ assume unsafeSlice :: Int -> Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeSlice :: Storable a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice i j = phony (unW . G.unsafeSlice i j)

{-@ assume unsafeTake :: Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeTake :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake n = phony (unW . G.unsafeTake n)

{-@ assume unsafeDrop :: Int -> x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeDrop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop n = phony (unW . G.unsafeDrop n)

{-@ assume unsafeInit :: x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeInit :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = phony (unW . G.unsafeInit)

{-@ assume unsafeTail :: x:Data.Vector.SEXP.Mutable.Internal.MVector s a -> TMVector s a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x) @-}
unsafeTail :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = phony (unW . G.unsafeTail)

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Storable a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = phony2 G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
{-@ assume new :: t:R.SEXPTYPE -> Int -> m (TMVector (Region m) a t) @-}
new :: (MonadR m, Storable a)
    => R.SEXPTYPE
    -> Int
    -> m (MVector (Region m) a)
{-# INLINE new #-}
new t n = do
    acquireIO <- getAcquireIO
    reify (acquireIO, pure t) $ proxyW $ G.new n

-- | Create a mutable vector of the given length. The length is not checked.
{-@ assume unsafeNew :: t:R.SEXPTYPE -> Int -> m (TMVector (Region m) a t) @-}
unsafeNew :: (MonadR m, Storable a) => R.SEXPTYPE -> Int -> m (MVector (Region m) a)
{-# INLINE unsafeNew #-}
unsafeNew t n = do
    acquireIO <- getAcquireIO
    reify (acquireIO, pure t) $ proxyW $ G.unsafeNew n

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
{-@ assume replicate :: t:R.SEXPTYPE -> Int -> a -> m (TMVector (Region m) a t) @-}
replicate :: (MonadR m, Storable a) => R.SEXPTYPE -> Int -> a -> m (MVector (Region m) a)
{-# INLINE replicate #-}
replicate t n x = do
    acquireIO <- getAcquireIO
    reify (acquireIO, pure t) $ proxyW $ G.replicate n x

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
{-@ assume replicateM :: t:R.SEXPTYPE -> Int -> m a -> m (TMVector (Region m) a t) @-}
replicateM :: (MonadR m, Storable a) => R.SEXPTYPE -> Int -> m a -> m (MVector (Region m) a)
{-# INLINE replicateM #-}
replicateM t n m = do
    acquireIO <- getAcquireIO
    reify (acquireIO, pure t) $ proxyW $ G.replicateM n m

-- | Create a copy of a mutable vector.
{-@ assume clone :: x:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a -> m (TMVector (Region m) a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)) @-}
{-@ ignore clone @-}
clone :: (MonadR m, Storable a)
      => MVector (Region m) a
      -> m (MVector (Region m) a)
{-# INLINE clone #-}
clone v = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ proxyW $ G.clone (W v)

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (MonadR m, Storable a) => MVector (Region m) a -> m ()
{-# INLINE clear #-}
clear v = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.clear (withW p v)

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (MonadR m, Storable a)
     => MVector (Region m) a -> Int -> m a
{-# INLINE read #-}
read v i = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.read (withW p v) i

-- | Replace the element at the given position.
write :: (MonadR m, Storable a)
      => MVector (Region m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.write (withW p v) i x

-- | Swap the elements at the given positions.
swap :: (MonadR m, Storable a)
     => MVector (Region m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap v i j = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.swap (withW p v) i j

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (MonadR m, Storable a)
           => MVector (Region m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v i = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.unsafeRead (withW p v) i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (MonadR m, Storable a)
            => MVector (Region m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v i x = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.unsafeWrite (withW p v) i x

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (MonadR m, Storable a)
           => MVector (Region m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap v i j = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.unsafeSwap (withW p v) i j

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (MonadR m, Storable a) => MVector (Region m) a -> a -> m ()
{-# INLINE set #-}
set v x = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v)) $ \p -> G.set (withW p v) x

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
{-@
assume copy
  :: (MonadR m, Storable a)
  => x:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a
  -> TMVector (Region m) a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)
  -> m ()
@-}
copy :: (MonadR m, Storable a)
     => MVector (Region m) a
     -> MVector (Region m) a
     -> m ()
{-# INLINE copy #-}
copy v1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v1)) $ \p -> G.copy (withW p v1) (withW p v2)

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
{-@
assume unsafeCopy
  :: (MonadR m, Storable a)
  => x:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a
  -> TMVector (Region m) a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)
  -> m ()
@-}
unsafeCopy :: (MonadR m, Storable a)
           => MVector (Region m) a   -- ^ target
           -> MVector (Region m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy v1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v1)) $ \p -> G.unsafeCopy (withW p v1) (withW p v2)

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
{-@
assume move
  :: (MonadR m, Storable a)
  => x:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a
  -> TMVector (Region m) a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)
  -> m ()
@-}
move :: (MonadR m, Storable a)
     => MVector (Region m) a
     -> MVector (Region m) a
     -> m ()
{-# INLINE move #-}
move v1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v1)) $ \p -> G.move (withW p v1) (withW p v2)

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
{-@
assume unsafeMove
  :: x:Data.Vector.SEXP.Mutable.Internal.MVector (Region m) a
  -> TMVector (Region m) a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf x)
  -> m ()
@-}
unsafeMove :: (MonadR m, Storable a)
           => MVector (Region m) a             -- ^ target
           -> MVector (Region m) a             -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove v1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (mvtypeOf v1)) $ \p -> G.unsafeMove (withW p v1) (withW p v2)
