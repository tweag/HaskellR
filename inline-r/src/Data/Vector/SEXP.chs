-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are an instance of "Data.Vector.Storable", where the memory is
-- allocated from the R heap, in such a way that they can be converted to
-- a 'SEXP' through simple pointer arithmetic (see 'toSEXP') /in constant time/.
--
-- The main difference between "Data.Vector.SEXP" and "Data.Vector.Storable" is
-- that the former uses a header-prefixed data layout (the header immediately
-- precedes the payload of the vector). This means that no additional pointer
-- dereferencing is needed to reach the vector data. The trade-off is that most
-- slicing operations are O(N) instead of O(1).
--
-- If you make heavy use of slicing, then it's best to convert to
-- a "Data.Vector.Storable" vector first, using 'unsafeToStorable'.
--
-- Note that since 'unstream' relies on slicing operations, it will still be an
-- O(N) operation but it will copy vector data twice (instead of once).

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP
  ( Vector(..)
  , Mutable.MVector(..)
  , ElemRep
  , VECTOR
  , Data.Vector.SEXP.fromSEXP
  , unsafeFromSEXP
  , Data.Vector.SEXP.toSEXP
  , unsafeToSEXP

  -- * Accessors
  -- ** Length information
  , length
  , null
  -- ** Indexing
  , (!)
  , (!?)
  , head
  , last
  , unsafeIndex
  , unsafeHead
  , unsafeLast
  -- ** Monadic indexing
  , indexM
  , headM
  , lastM
  , unsafeIndexM
  , unsafeHeadM
  , unsafeLastM
  -- ** Extracting subvectors (slicing)
  , slice
  , init
  , take
  , drop
  , tail
  , splitAt
  , unsafeTail
  , unsafeSlice
  , unsafeDrop
  , unsafeTake
  , unsafeInit

  -- * Construction
  -- ** Initialisation
  , empty
  , singleton
  , replicate
  , generate
  , iterateN
  -- ** Monadic initialisation
  , replicateM
  , generateM
  , create
  -- ** Unfolding
  , unfoldr
  , unfoldrN
  , constructN
  , constructrN
  -- ** Enumeration
  , enumFromN
  , enumFromStepN
  , enumFromTo
  , enumFromThenTo
  -- ** Concatenation
  , cons
  , snoc
  , (++)
  , concat

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
  , map
  , imap
  , concatMap

  -- ** Monadic mapping
  , mapM
  , mapM_
  , forM
  , forM_

  -- ** Zipping
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , izipWith
  , izipWith3
  , izipWith4
  , izipWith5
  , izipWith6

  -- ** Monadic zipping
  {-, zipWithM-}, zipWithM_

  -- * Working with predicates

  -- ** Filtering
  , filter
  , ifilter
  , filterM
  , takeWhile
  , dropWhile

  -- ** Partitioning
  , partition
  , unstablePartition
  , span
  , break

  -- ** Searching
  , elem
  , notElem
  , find
  , findIndex
  , {-findIndices,-} elemIndex {-, elemIndices -}

  -- * Folding
  , foldl
  , foldl1
  , foldl'
  , foldl1'
  , foldr
  , foldr1
  , foldr'
  , foldr1'
  , ifoldl
  , ifoldl'
  , ifoldr
  , ifoldr'

  -- ** Specialised folds
  , all
  , any
  , and
  , or
  , sum
  , product
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , minIndex
  , minIndexBy
  , maxIndex
  , maxIndexBy

  -- ** Monadic folds
  , foldM
  , foldM'
  , fold1M
  , fold1M'
  , foldM_
  , foldM'_
  , fold1M_
  , fold1M'_

  -- * Prefix sums (scans)
  , prescanl
  , prescanl'
  , postscanl
  , postscanl'
  , scanl
  , scanl'
  , scanl1
  , scanl1'
  , prescanr
  , prescanr'
  , postscanr
  , postscanr'
  , scanr
  , scanr'
  , scanr1
  , scanr1'

  -- * Conversions
  -- ** Lists
  , toList
  , fromList
  , fromListN
  -- ** Mutable vectors
  , freeze
  , thaw
  , copy
  , unsafeFreeze
  , unsafeThaw
  , unsafeCopy

  -- ** SEXP specific
  , toString
  , toByteString
  , fromStorable
  , unsafeToStorable
  ) where

import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable (MVector(..))
import qualified Data.Vector.SEXP.Mutable as Mutable
import Foreign.R ( SEXP )
import qualified Foreign.R as R
import Foreign.R.Type ( SEXPTYPE(Char) )

import Control.Monad.Primitive ( PrimMonad, PrimState )
import Control.Monad.ST (ST)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Storable as Storable
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as B

import Control.Applicative ((<$>))
import Control.Monad ( liftM )
import Control.Monad.Primitive ( unsafeInlineIO, unsafePrimToPrim )
import Data.Word ( Word8 )
-- import Data.Int  ( Int32 )
import Foreign ( Ptr, plusPtr, castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array ( copyArray )
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif

import Prelude
  ( Eq(..)
  , Enum
  , Monad(..)
  , Num(..)
  , Ord(..)
  , Show(..)
  , Bool
  , Int
  , IO
  , Maybe
  , Ordering
  , String
  , (.)
  , ($)
  , ($!)
  , (=<<)
  , all
  , and
  , any
  , fromIntegral
  , or
  , seq
  , uncurry
  )
import qualified Prelude

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

-- | Immutable vectors. The second type paramater is a phantom parameter
-- reflecting at the type level the tag of the vector when viewed as a 'SEXP'.
-- The tag of the vector and the representation type are related via 'ElemRep'.
newtype Vector s (ty :: SEXPTYPE) a = Vector { unVector :: SEXP s ty }

type instance G.Mutable (Vector r ty) = MVector r ty

instance (Eq a, VECTOR s ty a) => Eq (Vector s ty a) where
  a == b = toList a == toList b

instance (Show a, VECTOR s ty a)  => Show (Vector s ty a) where
  show v = "fromList " Prelude.++ showList (toList v) ""

instance (VECTOR s ty a)
         => G.Vector (Vector s ty) a where
  basicUnsafeFreeze (MVector s)  = return (Vector s)
  basicUnsafeThaw   (Vector s)   = return (MVector s)
  basicLength       (Vector s)   =
      unsafeInlineIO $
      fromIntegral <$> -- ({# get VECSEXP->vecsxp.length #} (R.unsexp s) :: IO Int32)
        ((\ ptr -> do { peekByteOff ptr 32 :: IO CInt }) (R.unsexp s))
  -- XXX Basic unsafe slice is O(N) complexity as it allocates a copy of
  -- a vector, due to limitations of R's VECSXP structure, which we reuse
  -- directly.
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

#if __GLASGOW_HASKELL__ >= 708
instance VECTOR s ty a => Exts.IsList (Vector s ty a) where
  type Item (Vector s ty a) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList
#endif

toVecPtr :: Vector s ty a -> Ptr a
toVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unVector mv)

toMVecPtr :: MVector s ty r a -> Ptr a
toMVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unMVector mv)

-- | /O(n)/ Create an immutable vector from a 'SEXP'. Because 'SEXP's are
-- mutable, this function yields an immutable /copy/ of the 'SEXP'.
fromSEXP :: (VECTOR s ty a, PrimMonad m)
         => SEXP s ty
         -> m (Vector s ty a)
fromSEXP s = G.freeze (Mutable.fromSEXP s)

-- | /O(1)/ Unsafe convert a mutable 'SEXP' to an immutable vector without
-- copying. The mutable vector must not be used after this operation, lest one
-- runs the risk of breaking referential transparency.
unsafeFromSEXP :: VECTOR s ty a
               => SEXP s ty
               -> Vector s ty a
unsafeFromSEXP s = Vector s

-- | /O(n)/ Yield a (mutable) copy of the vector as a 'SEXP'.
toSEXP :: (VECTOR s ty a, PrimMonad m)
       => Vector s ty a
       -> m (SEXP s ty)
toSEXP = liftM Mutable.toSEXP . G.thaw

-- | /O(1)/ Unsafely convert an immutable vector to a (mutable) 'SEXP' without
-- copying. The immutable vector must not be used after this operation.
unsafeToSEXP :: (VECTOR s ty a, PrimMonad m)
             => Vector s ty a
             -> m (SEXP s ty)
unsafeToSEXP = liftM Mutable.toSEXP . G.unsafeThaw

-- | /O(n)/ Convert a character vector into a 'String'.
toString :: Vector s 'Char Word8 -> String
toString v = unsafeInlineIO $ peekCString . castPtr
         . R.unsafeSEXPToVectorPtr
         . unVector $ v

-- | /O(1)/ Convert a character vector into a strict 'ByteString'.
toByteString :: Vector s 'Char Word8 -> ByteString
toByteString v@(Vector p) = unsafeInlineIO
        $ B.unsafePackCStringLen (castPtr $! R.unsafeSEXPToVectorPtr p, G.length v)

------------------------------------------------------------------------
-- Vector API
--

------------------------------------------------------------------------
-- Length
------------------------------------------------------------------------

-- | /O(1)/ Yield the length of the vector.
length :: VECTOR s ty a => Vector s ty a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector if empty
null :: VECTOR s ty a => Vector s ty a -> Bool
{-# INLINE null #-}
null = G.null


------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(1) Indexing
(!) :: VECTOR s ty a => Vector s ty a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing
(!?) :: VECTOR s ty a => Vector s ty a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element
head :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element
last :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: VECTOR s ty a => Vector s ty a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: VECTOR s ty a => Vector s ty a -> a
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
indexM :: (VECTOR s ty a, Monad m) => Vector s ty a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (VECTOR s ty a, Monad m) => Vector s ty a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

------------------------------------------------------------------------
-- Extracting subvectors (slicing)
------------------------------------------------------------------------

-- | /O(N)/ Yield a slice of the vector with copying it. The vector must
-- contain at least @i+n@ elements.
slice :: VECTOR s ty a
      => Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector s ty a
      -> Vector s ty a
{-# INLINE slice #-}
slice = G.slice

-- | /O(N)/ Yield all but the last element, this operation will copy an array.
-- The vector may not be empty.
init :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE init #-}
init = G.init

-- | /O(N)/ Copy all but the first element. The vector may not be empty.
tail :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE tail #-}
tail = G.tail

-- | /O(N)/ Yield at the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE take #-}
take = G.take

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE drop #-}
drop = G.drop

-- | /O(N)/ Yield the first @n@ elements paired with the remainder with copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: VECTOR s ty a => Int -> Vector s ty a -> (Vector s ty a, Vector s ty a)
splitAt = G.splitAt

-- | /O(N)/ Yield a slice of the vector with copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: VECTOR s ty a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector s ty a
                       -> Vector s ty a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(N)/ Yield all but the last element with copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(N)/ Yield all but the first element with copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(N)/ Yield the first @n@ elements with copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: VECTOR s ty a => Vector s ty a
{-# INLINE empty #-}
empty = G.empty -- TODO test

-- | /O(1)/ Vector with exactly one element
singleton :: VECTOR s ty a => a -> Vector s ty a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: VECTOR s ty a => Int -> a -> Vector s ty a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: VECTOR s ty a => Int -> (Int -> a) -> Vector s ty a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: VECTOR s ty a => Int -> (a -> a) -> a -> Vector s ty a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a Vector s ty by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: VECTOR s ty a => (b -> Maybe (a, b)) -> b -> Vector s ty a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: VECTOR s ty a => Int -> (b -> Maybe (a, b)) -> b -> Vector s ty a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: VECTOR s ty a => Int -> (Vector s ty a -> a) -> Vector s ty a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: VECTOR s ty a => Int -> (Vector s ty a -> a) -> Vector s ty a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (VECTOR s ty a, Num a) => a -> Int -> Vector s ty a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (VECTOR s ty a, Num a) => a -> a -> Int -> Vector s ty a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (VECTOR s ty a, Enum a) => a -> a -> Vector s ty a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (VECTOR s ty a, Enum a) => a -> a -> a -> Vector s ty a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: VECTOR s ty a => a -> Vector s ty a -> Vector s ty a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element
snoc :: VECTOR s ty a => Vector s ty a -> a -> Vector s ty a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: VECTOR s ty a => Vector s ty a -> Vector s ty a -> Vector s ty a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list
concat :: VECTOR s ty a => [Vector s ty a] -> Vector s ty a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, VECTOR s ty a) => Int -> m a -> m (Vector s ty a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, VECTOR s ty a) => Int -> (Int -> m a) -> m (Vector s ty a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: VECTOR s ty a => (forall r. ST r (MVector s ty r a)) -> Vector s ty a
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
force :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: VECTOR s ty a => Vector s ty a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector s ty a
{-# INLINE (//) #-}
(//) = (G.//)

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector s ty and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial Vector s ty at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
update_ :: VECTOR s ty a
        => Vector s ty a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector s ty a   -- ^ value vector (of length @n2@)
        -> Vector s ty a
{-# INLINE update_ #-}
update_ = G.update_
-}

{-
-- | Same as ('//') but without bounds checking.
unsafeUpd :: VECTOR s ty a => Vector s ty a -> [(Int, a)] -> Vector s ty a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd
-}

{-
-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: VECTOR s ty a => Vector s ty a -> Vector Int -> Vector s ty a -> Vector s ty a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_
-}

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: VECTOR s ty a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector s ty a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector s ty a
{-# INLINE accum #-}
accum = G.accum

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector s ty and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial Vector s ty at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
accumulate_ :: (VECTOR s ty a, VECTOR s ty b)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector s ty a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector s ty b      -- ^ value vector (of length @n2@)
            -> Vector s ty a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_
-}

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: VECTOR s ty a => (a -> b -> a) -> Vector s ty a -> [(Int,b)] -> Vector s ty a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

{-
-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (VECTOR s ty a, VECTOR s ty b) =>
               (a -> b -> a) -> Vector s ty a -> Vector Int -> Vector s ty b -> Vector s ty a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_
-}

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE reverse #-}
reverse = G.reverse

{-
-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index Vector s ty by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: VECTOR s ty a => Vector s ty a -> Vector Int -> Vector s ty a
{-# INLINE backpermute #-}
backpermute = G.backpermute
-}

{-
-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: VECTOR s ty a => Vector s ty a -> Vector Int -> Vector s ty a
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
modify :: VECTOR s ty a => (forall s. MVector s a -> ST s ()) -> Vector s ty a -> Vector s ty a
{-# INLINE modify #-}
modify p = G.modify p
-}

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (VECTOR s ty a, VECTOR s ty b) => (a -> b) -> Vector s ty a -> Vector s ty b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a Vector s ty and its index
imap :: (VECTOR s ty a, VECTOR s ty b) => (Int -> a -> b) -> Vector s ty a -> Vector s ty b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a Vector s ty and concatenate the results.
concatMap :: (VECTOR s ty a, VECTOR s ty b) => (a -> Vector s ty b) -> Vector s ty a -> Vector s ty b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, VECTOR s ty a, VECTOR s ty b) => (a -> m b) -> Vector s ty a -> m (Vector s ty b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to all elements of a Vector s ty and ignore the
-- results
mapM_ :: (Monad m, VECTOR s ty a) => (a -> m b) -> Vector s ty a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, VECTOR s ty a, VECTOR s ty b) => Vector s ty a -> (a -> m b) -> m (Vector s ty b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a Vector s ty and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, VECTOR s ty a) => Vector s ty a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
        => (a -> b -> c) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c
{-# INLINE zipWith #-}
zipWith f xs ys = G.unstream (Stream.zipWith f (G.stream xs) (G.stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d)
         => (a -> b -> c -> d) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = G.unstream (Stream.zipWith3 f (G.stream as) (G.stream bs) (G.stream cs))

zipWith4 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e)
         => (a -> b -> c -> d -> e)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds = G.unstream (Stream.zipWith4 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds))

zipWith5 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
             VECTOR s tyf f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
         -> Vector s tyf f
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es = G.unstream (Stream.zipWith5 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))

zipWith6 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
             VECTOR s tyf f, VECTOR s tyg g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
         -> Vector s tyf f -> Vector s tyg g
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs = G.unstream (Stream.zipWith6 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
         => (Int -> a -> b -> c) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c
{-# INLINE izipWith #-}
izipWith f as bs = G.unstream (Stream.zipWith (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs))

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d)
          => (Int -> a -> b -> c -> d)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs = G.unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs))

izipWith4 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds =  G.unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds))

izipWith5 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
              VECTOR s tyf f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
          -> Vector s tyf f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es =  G.unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))

izipWith6 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
              VECTOR s tyf f, VECTOR s tyg g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
          -> Vector s tyf f -> Vector s tyg g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs =  G.unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))

-- Monadic zipping
-- ---------------

{-
-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
         => (a -> b -> m c) -> Vector s tya a -> Vector s tyb b -> m (Vector s tyc c)
{-# INLINE zipWithM #-}
zipWithM f as bs = G.unstreamM (Stream.zipWithM f (G.stream as) (G.stream bs))
-}

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, VECTOR s tya a, VECTOR s tyb b)
          => (a -> b -> m c) -> Vector s tya a -> Vector s tyb b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f as bs = Stream.zipWithM_ f (G.stream as) (G.stream bs)

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: VECTOR s ty a => (Int -> a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, VECTOR s ty a) => (a -> m Bool) -> Vector s ty a -> m (Vector s ty a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- with copying.
takeWhile :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- with copying.
dropWhile :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest with copying.
span :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest with copying.
break :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

{-
-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices
-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

{-
-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices
-}

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: VECTOR s ty b => (a -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator
foldl' :: VECTOR s ty b => (a -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold
foldr :: VECTOR s ty a => (a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: VECTOR s ty a => (a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: VECTOR s ty b => (a -> Int -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: VECTOR s ty b => (a -> Int -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: VECTOR s ty a => (Int -> a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: VECTOR s ty a => (Int -> a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

{-
-- | /O(n)/ Check if all elements satisfy the predicate.
all :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
any :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Bool
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
sum :: (VECTOR s ty a, Num a) => Vector s ty a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the produce of the elements
product :: (VECTOR s ty a, Num a) => Vector s ty a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (VECTOR s ty a, Ord a) => Vector s ty a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the Vector s ty according to the given
-- comparison function. The vector may not be empty.
maximumBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (VECTOR s ty a, Ord a) => Vector s ty a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the Vector s ty according to the given
-- comparison function. The vector may not be empty.
minimumBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (VECTOR s ty a, Ord a) => Vector s ty a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the Vector s ty according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (VECTOR s ty a, Ord a) => Vector s ty a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the Vector s ty according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m ()
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
prescanl :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
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
postscanl :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
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
scanl :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left scan
postscanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: VECTOR s ty a => Vector s ty a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector
fromList :: VECTOR s ty a => [a] -> Vector s ty a
{-# INLINE fromList #-}
fromList xs = G.fromListN (Prelude.length xs) xs

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: VECTOR s ty a => Int -> [a] -> Vector s ty a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Unsafe casts
-- --------------------------

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one with
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
        :: (VECTOR s ty a, PrimMonad m) => MVector s ty (PrimState m) a -> m (Vector s ty a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one with
-- copying. The immutable vector may not be used after this operation.
unsafeThaw
        :: (VECTOR s ty a, PrimMonad m) => Vector s ty a -> m (MVector s ty (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (VECTOR s ty a, PrimMonad m) => Vector s ty a -> m (MVector s ty (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (VECTOR s ty a, PrimMonad m) => MVector s ty (PrimState m) a -> m (Vector s ty a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (VECTOR s ty a, PrimMonad m) => MVector s ty (PrimState m) a -> Vector s ty a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (VECTOR s ty a, PrimMonad m) => MVector s ty (PrimState m) a -> Vector s ty a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | O(1) Inplace convertion to Storable vector.
unsafeToStorable :: VECTOR s ty a
                 => Vector s ty a         -- ^ target
                 -> Storable.Vector a     -- ^ source
{-# INLINE unsafeToStorable #-}
unsafeToStorable v = unsafeInlineIO $
  G.unsafeFreeze =<< Mutable.unsafeToStorable =<< G.unsafeThaw v

-- | O(N) Convertion from storable vector to SEXP vector.
fromStorable :: VECTOR s ty a
             => Storable.Vector a
             -> Vector s ty a
{-# INLINE fromStorable #-}
fromStorable v = unsafeInlineIO $
  G.unsafeFreeze =<< Mutable.fromStorable =<< G.unsafeThaw v
