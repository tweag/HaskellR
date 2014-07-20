-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are a special instance of "Data.Vector.Storable" where the memory is
-- allocated from the R heap, and in such a way that they can be converted to
-- a 'SEXP' through simple pointer arithmetic (see 'toSEXP').
--
-- The main difference between 'Data.Vector.SEXP.Vector' and
-- 'Data.Vector.Storable.Vector' is that the former uses header-prefixed data layout.
-- This means that no additional pointer jump is needed to reach the vector data.
-- The trade-off is that all slicing operations are O(N) instead of O(1) and there
-- is no mutable instance of the SEXP vector.
--
-- Note that since 'unstream' relies on slicing operations, it will still be an O(N)
-- operation but it will copy vector data twice unlike most vector implementations.
--
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vector.SEXP
  ( Vector(..)
  , Mutable.MVector(..)
  , ElemRep
  , SexpVector
  , Data.Vector.SEXP.fromSEXP
  , unsafeFromSEXP
  , Data.Vector.SEXP.toSEXP
  , unsafeToSEXP

  -- * Accessors
  -- ** Length information
  , length, null
  -- ** Indexing
  , (!), (!?), head, last, unsafeIndex, unsafeHead, unsafeLast
  -- ** Monadic indexing
  , indexM, headM, lastM, unsafeIndexM, unsafeHeadM, unsafeLastM
  -- ** Extracting subvectors (slicing)
  , slice, init, take, drop, tail, splitAt, unsafeTail, unsafeSlice, unsafeDrop, unsafeTake, unsafeInit

  -- * Construction
  -- ** Initialisation
  , empty, singleton, replicate, generate, iterateN
  -- ** Monadic initialisation
  , replicateM, generateM, create
  -- ** Unfolding
  , unfoldr, unfoldrN
  , constructN, constructrN
  -- ** Enumeration
  , enumFromN, enumFromStepN, enumFromTo, enumFromThenTo
  -- ** Concatenation
  , cons, snoc, (++), concat

  -- ** Restricting memory usage
  , force

  -- * Modifying vectors

  -- ** Bulk updates
  , (//) -- , update_,
  -- unsafeUpd, unsafeUpdate_

  -- ** Accumulations
  , accum{-, accumulate_-}
  , unsafeAccum{-, unsafeAccumulate_-}

  -- ** Permutations
  , reverse{-, backpermute-}{-, unsafeBackpermute -}

  -- ** Safe destructive updates
  {-, modify-}

  -- * Elementwise operations

  -- ** Mapping
  , map, imap, concatMap

  -- ** Monadic mapping
  , mapM, mapM_, forM, forM_

  -- ** Zipping
  , zipWith, zipWith3, zipWith4, zipWith5, zipWith6
  , izipWith, izipWith3, izipWith4, izipWith5, izipWith6

  -- ** Monadic zipping
  {-, zipWithM-}, zipWithM_

  -- * Working with predicates

  -- ** Filtering
  , filter, ifilter, filterM
  , takeWhile, dropWhile

  -- ** Partitioning
  , partition, unstablePartition, span, break

  -- ** Searching
  , elem, notElem, find, findIndex, {-findIndices,-} elemIndex {-, elemIndices -}

  -- * Folding
  , foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1'
  , ifoldl, ifoldl', ifoldr, ifoldr'

  -- ** Specialised folds
  , all, any, and, or
  , sum, product
  , maximum, maximumBy, minimum, minimumBy
  , minIndex, minIndexBy, maxIndex, maxIndexBy

  -- ** Monadic folds
  , foldM, foldM', fold1M, fold1M'
  , foldM_, foldM'_, fold1M_, fold1M'_

  -- * Prefix sums (scans)
  , prescanl, prescanl'
  , postscanl, postscanl'
  , scanl, scanl', scanl1, scanl1'
  , prescanr, prescanr'
  , postscanr, postscanr'
  , scanr, scanr', scanr1, scanr1'

  -- * Conversions
  -- ** Lists
  , toList, fromList, fromListN
  -- ** T
  , unsafeFreeze, unsafeThaw, thaw, freeze, copy, unsafeCopy

  -- ** SEXP specific
  , toString
  , toByteString

  ) where

import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable (MVector(..))
import qualified Data.Vector.SEXP.Mutable as Mutable
import Foreign.R ( SEXP )
import qualified Foreign.R as R
import Foreign.R.Type ( SEXPTYPE(Char{-, Logical-}), IsVector )

import Control.Monad.Primitive ( PrimMonad, PrimState )
import Control.Monad.ST (ST)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as B
import Data.Singletons (SingI)

import Control.Applicative ((<$>))
import Control.Monad ( liftM )
import Control.Monad.Primitive ( unsafeInlineIO, unsafePrimToPrim )
import Data.Word ( Word8 )
import Foreign ( Ptr, plusPtr, castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array ( copyArray )

import Prelude hiding ( length, head, null, last, drop, tail, splitAt, init, take,
  foldl, foldl1, mapM_, mapM, concatMap,
  foldr, foldr1, product, maximum, minimum, scanr, scanr1, scanl, scanl1,
  dropWhile, takeWhile, filter, map, reverse, concat, (++), replicate, enumFromTo, enumFromThenTo,
  span, break, elem, notElem, zipWith, zipWith3, sum)
import qualified Prelude

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

newtype Vector (ty :: SEXPTYPE) a = Vector { unVector :: SEXP ty }

type instance G.Mutable (Vector ty) = MVector ty

instance (Eq a, SexpVector ty a) => Eq (Vector ty a) where
  a == b = toList a == toList b

instance (Show a, SexpVector ty a)  => Show (Vector ty a) where
  show v = "fromList " Prelude.++ showList (toList v) ""

instance (IsVector ty, Storable a, SingI ty, a ~ ElemRep ty)
         => G.Vector (Vector ty) a where
  basicUnsafeFreeze (MVector s)  = return (Vector s)
  basicUnsafeThaw   (Vector s)   = return (MVector s)
  basicLength       (Vector s)   =
      unsafeInlineIO $
      fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp s)
  -- | Basic unsafe slice is O(N) complexity as it allocates a copy of vector,
  basicUnsafeSlice i l v         = unsafeInlineIO $ do
    mv <- Mutable.new l
    copyArray (toVecPtr v `plusPtr` i)
              (toMVecPtr mv)
              l
    G.basicUnsafeFreeze mv
  basicUnsafeIndexM v i          = return . unsafeInlineIO
                                 $ peekElemOff (toVecPtr v) i
  basicUnsafeCopy   mv v         = unsafePrimToPrim $
    copyArray (toVecPtr v)
              (toMVecPtr mv)
              (G.basicLength v)

  elemseq _                      = seq

toVecPtr :: Vector ty a -> Ptr a
toVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unVector mv)

toMVecPtr :: MVector ty s a -> Ptr a
toMVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unMVector mv)

-- | /O(n)/ Create an immutable vector from a 'SEXP'. Because 'SEXP's are
-- mutable, this function yields an immutable copy of the 'SEXP'.
fromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
         => SEXP ty
         -> m (Vector ty a)
fromSEXP s = return (Vector s)       -- G.freeze =<< Mutable.fromSEXP s

-- | /O(1)/ Unsafe convert a mutable 'SEXP' to an immutable vector with
-- copying. The mutable vector must not be used after this operation, lest one
-- runs the risk of breaking referential transparency.
unsafeFromSEXP :: (E ty a, Storable a, IsVector ty, PrimMonad m)
               => SEXP ty
               -> m (Vector ty a)
unsafeFromSEXP s = return (Vector s) -- G.unsafeFreeze =<< Mutable.fromSEXP s

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
toString v = unsafeInlineIO $ peekCString . castPtr
         . R.unsafeSEXPToVectorPtr
         . unVector $ v

-- | /O(1)/ Convert a character vector into a strict 'ByteString'.
toByteString :: Vector 'Char Word8 -> ByteString
toByteString v@(Vector p) = unsafeInlineIO
        $ B.unsafePackCStringLen (castPtr $! R.unsafeSEXPToVectorPtr p, G.length v)

type SexpVector ty a = (Storable a, IsVector ty, SingI ty, ElemRep ty ~ a)

------------------------------------------------------------------------
-- Vector API
--

------------------------------------------------------------------------
-- Length
------------------------------------------------------------------------

-- | /O(1)/ Yield the length of the vector.
length :: SexpVector ty a => Vector ty a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector if empty
null :: SexpVector ty a => Vector ty a -> Bool
{-# INLINE null #-}
null = G.null


------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(1) Indexing
(!) :: SexpVector ty a => Vector ty a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing
(!?) :: SexpVector ty a => Vector ty a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element
head :: SexpVector ty a => Vector ty a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element
last :: SexpVector ty a => Vector ty a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: SexpVector ty a => Vector ty a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: SexpVector ty a => Vector ty a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: SexpVector ty a => Vector ty a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

------------------------------------------------------------------------
-- Monadic indexing
------------------------------------------------------------------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- elements) is evaluated eagerly.
--
indexM :: (SexpVector ty a, Monad m) => Vector ty a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (SexpVector ty a, Monad m) => Vector ty a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (SexpVector ty a, Monad m) => Vector ty a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (SexpVector ty a, Monad m) => Vector ty a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (SexpVector ty a, Monad m) => Vector ty a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (SexpVector ty a, Monad m) => Vector ty a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

------------------------------------------------------------------------
-- Extracting subvectors (slicing)
------------------------------------------------------------------------

-- | /O(N)/ Yield a slice of the vector with copying it. The vector must
-- contain at least @i+n@ elements.
slice :: SexpVector ty a
      => Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector ty a
      -> Vector ty a
{-# INLINE slice #-}
slice = G.slice

-- | /O(N)/ Yield all but the last element, this operation will copy an array.
-- The vector may not be empty.
init :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE init #-}
init = G.init

-- | /O(N)/ Copy all but the first element. The vector may not be empty.
tail :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE tail #-}
tail = G.tail

-- | /O(N)/ Yield at the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: SexpVector ty a => Int -> Vector ty a -> Vector ty a
{-# INLINE take #-}
take = G.take

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: SexpVector ty a => Int -> Vector ty a -> Vector ty a
{-# INLINE drop #-}
drop = G.drop

-- | /O(N)/ Yield the first @n@ elements paired with the remainder with copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: SexpVector ty a => Int -> Vector ty a -> (Vector ty a, Vector ty a)
splitAt = G.splitAt

-- | /O(N)/ Yield a slice of the vector with copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: SexpVector ty a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector ty a
                       -> Vector ty a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(N)/ Yield all but the last element with copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(N)/ Yield all but the first element with copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(N)/ Yield the first @n@ elements with copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: SexpVector ty a => Int -> Vector ty a -> Vector ty a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: SexpVector ty a => Int -> Vector ty a -> Vector ty a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: SexpVector ty a => Vector ty a
{-# INLINE empty #-}
empty = G.empty -- TODO test

-- | /O(1)/ Vector with exactly one element
singleton :: SexpVector ty a => a -> Vector ty a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: SexpVector ty a => Int -> a -> Vector ty a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: SexpVector ty a => Int -> (Int -> a) -> Vector ty a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: SexpVector ty a => Int -> (a -> a) -> a -> Vector ty a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a Vector ty by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: SexpVector ty a => (b -> Maybe (a, b)) -> b -> Vector ty a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: SexpVector ty a => Int -> (b -> Maybe (a, b)) -> b -> Vector ty a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: SexpVector ty a => Int -> (Vector ty a -> a) -> Vector ty a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: SexpVector ty a => Int -> (Vector ty a -> a) -> Vector ty a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (SexpVector ty a, Num a) => a -> Int -> Vector ty a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (SexpVector ty a, Num a) => a -> a -> Int -> Vector ty a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (SexpVector ty a, Enum a) => a -> a -> Vector ty a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (SexpVector ty a, Enum a) => a -> a -> a -> Vector ty a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: SexpVector ty a => a -> Vector ty a -> Vector ty a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element
snoc :: SexpVector ty a => Vector ty a -> a -> Vector ty a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: SexpVector ty a => Vector ty a -> Vector ty a -> Vector ty a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list
concat :: SexpVector ty a => [Vector ty a] -> Vector ty a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, SexpVector ty a) => Int -> m a -> m (Vector ty a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, SexpVector ty a) => Int -> (Int -> m a) -> m (Vector ty a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: SexpVector ty a => (forall s. ST s (MVector ty s a)) -> Vector ty a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p


-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: SexpVector ty a => Vector ty a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector ty a
{-# INLINE (//) #-}
(//) = (G.//)

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector ty and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial Vector ty at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
update_ :: SexpVector ty a
        => Vector ty a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector ty a   -- ^ value vector (of length @n2@)
        -> Vector ty a
{-# INLINE update_ #-}
update_ = G.update_
-}

{-
-- | Same as ('//') but without bounds checking.
unsafeUpd :: SexpVector ty a => Vector ty a -> [(Int, a)] -> Vector ty a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd
-}

{-
-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: SexpVector ty a => Vector ty a -> Vector Int -> Vector ty a -> Vector ty a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_
-}

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: SexpVector ty a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector ty a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector ty a
{-# INLINE accum #-}
accum = G.accum

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector ty and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial Vector ty at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
accumulate_ :: (SexpVector ty a, SexpVector ty b)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector ty a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector ty b      -- ^ value vector (of length @n2@)
            -> Vector ty a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_
-}

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: SexpVector ty a => (a -> b -> a) -> Vector ty a -> [(Int,b)] -> Vector ty a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

{-
-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (SexpVector ty a, SexpVector ty b) =>
               (a -> b -> a) -> Vector ty a -> Vector Int -> Vector ty b -> Vector ty a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_
-}

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: SexpVector ty a => Vector ty a -> Vector ty a
{-# INLINE reverse #-}
reverse = G.reverse

{-
-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index Vector ty by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: SexpVector ty a => Vector ty a -> Vector Int -> Vector ty a
{-# INLINE backpermute #-}
backpermute = G.backpermute
-}

{-
-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: SexpVector ty a => Vector ty a -> Vector Int -> Vector ty a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute
-}

-- Safe destructive updates
-- ------------------------

{-
-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> write v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: SexpVector ty a => (forall s. MVector s a -> ST s ()) -> Vector ty a -> Vector ty a
{-# INLINE modify #-}
modify p = G.modify p
-}

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (SexpVector ty a, SexpVector ty b) => (a -> b) -> Vector ty a -> Vector ty b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a Vector ty and its index
imap :: (SexpVector ty a, SexpVector ty b) => (Int -> a -> b) -> Vector ty a -> Vector ty b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a Vector ty and concatenate the results.
concatMap :: (SexpVector ty a, SexpVector ty b) => (a -> Vector ty b) -> Vector ty a -> Vector ty b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, SexpVector ty a, SexpVector ty b) => (a -> m b) -> Vector ty a -> m (Vector ty b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to all elements of a Vector ty and ignore the
-- results
mapM_ :: (Monad m, SexpVector ty a) => (a -> m b) -> Vector ty a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, SexpVector ty a, SexpVector ty b) => Vector ty a -> (a -> m b) -> m (Vector ty b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a Vector ty and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, SexpVector ty a) => Vector ty a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c)
        => (a -> b -> c) -> Vector tya a -> Vector tyb b -> Vector tyc c
{-# INLINE zipWith #-}
zipWith f xs ys = G.unstream (Stream.zipWith f (G.stream xs) (G.stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d)
         => (a -> b -> c -> d) -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = G.unstream (Stream.zipWith3 f (G.stream as) (G.stream bs) (G.stream cs))

zipWith4 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e)
         => (a -> b -> c -> d -> e)
         -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds = G.unstream (Stream.zipWith4 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds))

zipWith5 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e,
             SexpVector tyf f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
         -> Vector tyf f
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es = G.unstream (Stream.zipWith5 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))

zipWith6 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e,
             SexpVector tyf f, SexpVector tyg g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
         -> Vector tyf f -> Vector tyg g
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs = G.unstream (Stream.zipWith6 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c)
         => (Int -> a -> b -> c) -> Vector tya a -> Vector tyb b -> Vector tyc c
{-# INLINE izipWith #-}
izipWith f as bs = G.unstream (Stream.zipWith (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs))

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d)
          => (Int -> a -> b -> c -> d)
          -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs = G.unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs))

izipWith4 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds =  G.unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds))

izipWith5 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e,
              SexpVector tyf f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
          -> Vector tyf f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es =  G.unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))

izipWith6 :: (SexpVector tya a, SexpVector tyb b, SexpVector tyc c, SexpVector tyd d, SexpVector tye e,
              SexpVector tyf f, SexpVector tyg g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector tya a -> Vector tyb b -> Vector tyc c -> Vector tyd d -> Vector tye e
          -> Vector tyf f -> Vector tyg g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs =  G.unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))

-- Monadic zipping
-- ---------------

{-
-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, SexpVector tya a, SexpVector tyb b, SexpVector tyc c)
         => (a -> b -> m c) -> Vector tya a -> Vector tyb b -> m (Vector tyc c)
{-# INLINE zipWithM #-}
zipWithM f as bs = G.unstreamM (Stream.zipWithM f (G.stream as) (G.stream bs))
-}

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, SexpVector tya a, SexpVector tyb b)
          => (a -> b -> m c) -> Vector tya a -> Vector tyb b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f as bs = Stream.zipWithM_ f (G.stream as) (G.stream bs)

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Vector ty a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: SexpVector ty a => (Int -> a -> Bool) -> Vector ty a -> Vector ty a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, SexpVector ty a) => (a -> m Bool) -> Vector ty a -> m (Vector ty a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- with copying.
takeWhile :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Vector ty a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- with copying.
dropWhile :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Vector ty a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: SexpVector ty a => (a -> Bool) -> Vector ty a -> (Vector ty a, Vector ty a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: SexpVector ty a => (a -> Bool) -> Vector ty a -> (Vector ty a, Vector ty a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest with copying.
span :: SexpVector ty a => (a -> Bool) -> Vector ty a -> (Vector ty a, Vector ty a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest with copying.
break :: SexpVector ty a => (a -> Bool) -> Vector ty a -> (Vector ty a, Vector ty a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (SexpVector ty a, Eq a) => a -> Vector ty a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (SexpVector ty a, Eq a) => a -> Vector ty a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

{-
-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices
-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (SexpVector ty a, Eq a) => a -> Vector ty a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

{-
-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (SexpVector ty a, Eq a) => a -> Vector ty a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices
-}

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: SexpVector ty b => (a -> b -> a) -> a -> Vector ty b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator
foldl' :: SexpVector ty b => (a -> b -> a) -> a -> Vector ty b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold
foldr :: SexpVector ty a => (a -> b -> b) -> b -> Vector ty a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: SexpVector ty a => (a -> b -> b) -> b -> Vector ty a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: SexpVector ty b => (a -> Int -> b -> a) -> a -> Vector ty b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: SexpVector ty b => (a -> Int -> b -> a) -> a -> Vector ty b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: SexpVector ty a => (Int -> a -> b -> b) -> b -> Vector ty a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: SexpVector ty a => (Int -> a -> b -> b) -> b -> Vector ty a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

{-
-- | /O(n)/ Check if all elements satisfy the predicate.
all :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
any :: SexpVector ty a => (a -> Bool) -> Vector ty a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'
and :: Vector 'Logical Bool -> R.Logical
{-# INLINE and #-}
and = G.and -- FIXME

-- | /O(n)/ Check if any element is 'True'
or :: Vector 'Logical Bool -> R.Logical
{-# INLINE or #-}
or = G.or
-}

-- | /O(n)/ Compute the sum of the elements
sum :: (SexpVector ty a, Num a) => Vector ty a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the produce of the elements
product :: (SexpVector ty a, Num a) => Vector ty a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (SexpVector ty a, Ord a) => Vector ty a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the Vector ty according to the given
-- comparison function. The vector may not be empty.
maximumBy :: SexpVector ty a => (a -> a -> Ordering) -> Vector ty a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (SexpVector ty a, Ord a) => Vector ty a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the Vector ty according to the given
-- comparison function. The vector may not be empty.
minimumBy :: SexpVector ty a => (a -> a -> Ordering) -> Vector ty a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (SexpVector ty a, Ord a) => Vector ty a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the Vector ty according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: SexpVector ty a => (a -> a -> Ordering) -> Vector ty a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (SexpVector ty a, Ord a) => Vector ty a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the Vector ty according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: SexpVector ty a => (a -> a -> Ordering) -> Vector ty a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, SexpVector ty b) => (a -> b -> m a) -> a -> Vector ty b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, SexpVector ty a) => (a -> a -> m a) -> Vector ty a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, SexpVector ty b) => (a -> b -> m a) -> a -> Vector ty b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, SexpVector ty a) => (a -> a -> m a) -> Vector ty a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, SexpVector ty b) => (a -> b -> m a) -> a -> Vector ty b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, SexpVector ty a) => (a -> a -> m a) -> Vector ty a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, SexpVector ty b) => (a -> b -> m a) -> a -> Vector ty b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, SexpVector ty a) => (a -> a -> m a) -> Vector ty a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Prefix sums (scans)
-- -------------------

-- | /O(n)/ Prescan
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
scanl :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> a) -> a -> Vector ty b -> Vector ty a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> Vector ty a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> Vector ty a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left scan
postscanr :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (SexpVector ty a, SexpVector ty b) => (a -> b -> b) -> b -> Vector ty a -> Vector ty b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> Vector ty a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: SexpVector ty a => (a -> a -> a) -> Vector ty a -> Vector ty a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: SexpVector ty a => Vector ty a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector
fromList :: SexpVector ty a => [a] -> Vector ty a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: SexpVector ty a => Int -> [a] -> Vector ty a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Unsafe casts
-- --------------------------

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one with
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
        :: (SexpVector ty a, PrimMonad m) => MVector ty (PrimState m) a -> m (Vector ty a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one with
-- copying. The immutable vector may not be used after this operation.
unsafeThaw
        :: (SexpVector ty a, PrimMonad m) => Vector ty a -> m (MVector ty (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (SexpVector ty a, PrimMonad m) => Vector ty a -> m (MVector ty (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (SexpVector ty a, PrimMonad m) => MVector ty (PrimState m) a -> m (Vector ty a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (SexpVector ty a, PrimMonad m) => MVector ty (PrimState m) a -> Vector ty a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (SexpVector ty a, PrimMonad m) => MVector ty (PrimState m) a -> Vector ty a -> m ()
{-# INLINE copy #-}
copy = G.copy
