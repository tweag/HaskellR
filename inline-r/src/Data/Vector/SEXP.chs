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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
  , (//)
  -- , update_
  , unsafeUpd
  -- , unsafeUpdate_

  -- ** Accumulations
  , accum
  -- , accumulate_
  , unsafeAccum
  -- , unsafeAccumulate_

  -- ** Permutations
  , reverse
  -- , backpermute
  -- , unsafeBackpermute

  -- ** Safe destructive updates
  -- , modify

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
  , zipWithM
  , zipWithM_

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
  -- , findIndices
  , elemIndex
  -- , elemIndices

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
  -- , and
  -- , or
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

  -- ** SEXP specific helpers.
  , toString
  , toByteString
  ) where

import Control.Monad.R.Class
import Control.Monad.R.Internal
import Control.Memory.Region
import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable (MVector)
import qualified Data.Vector.SEXP.Mutable as Mutable
import qualified Data.Vector.SEXP.Mutable.Internal as Mutable
import Foreign.R ( SEXP(..) )
import qualified Foreign.R as R
import Foreign.R.Type ( SEXPTYPE(Char) )

import Control.Monad.Primitive ( PrimMonad )
import Control.Monad.ST (ST, runST)
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..), reify)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.New (run)
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B

import Control.Applicative hiding (empty)
import Control.Monad.Primitive ( unsafeInlineIO, unsafePrimToPrim )
import Data.Word ( Word8 )
import Foreign ( Ptr, plusPtr, castPtr, peekElemOff )
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Array ( copyArray )
import qualified GHC.Foreign as GHC
import qualified GHC.ForeignPtr as GHC
import GHC.IO.Encoding.UTF8
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif
import System.IO.Unsafe

import Prelude
  ( Eq(..)
  , Enum
  , Monad(..)
  , Num(..)
  , Ord(..)
  , Show(..)
  , Bool
  , IO
  , Maybe
  , Ordering
  , String
  , (.)
  , ($)
  , fromIntegral
  , seq
  , uncurry
  )
import qualified Prelude

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

newtype ForeignSEXP (ty::SEXPTYPE) = ForeignSEXP (ForeignPtr ())

-- | Create a 'ForeignSEXP' from 'SEXP'.
foreignSEXP :: PrimMonad m => SEXP s ty -> m (ForeignSEXP ty)
foreignSEXP sx@(SEXP ptr) =
    unsafePrimToPrim $ do
      R.preserveObject sx
      ForeignSEXP <$> GHC.newConcForeignPtr (castPtr ptr) (R.releaseObject sx)

{-
-- | 'ForeignSEXP' deconstructor. Like with 'withForeignPtr' it's not safe to use
-- 'SEXP s ty' outside of 'withForeinSEXP'.
withForeignSEXP
  :: MonadR m
  => ForeignSEXP ty
  -> (forall s . SEXP s ty -> IO r)
  -> m r
withForeignSEXP (ForeignSEXP fptr) f =
    io $ withForeignPtr fptr $ \ptr -> f (SEXP (castPtr ptr))
-}

withForeignSEXP1
  ::  ForeignSEXP ty
  -> (SEXP s ty -> IO r)
  -> IO r
withForeignSEXP1 (ForeignSEXP fptr) f =
    withForeignPtr fptr $ \ptr -> f (SEXP (castPtr ptr))

-- | Immutable vectors. The second type paramater is a phantom parameter
-- reflecting at the type level the tag of the vector when viewed as a 'SEXP'.
-- The tag of the vector and the representation type are related via 'ElemRep'.
data Vector s (ty :: SEXPTYPE) a = Vector
  { vectorBase   :: {-# UNPACK #-} !(ForeignSEXP ty)
  , vectorOffset :: {-# UNPACK #-} !Int32
  , vectorLength :: {-# UNPACK #-} !Int32
  }

instance (Eq a, VECTOR s ty a) => Eq (Vector s ty a) where
  a == b = toList a == toList b

instance (Show a, VECTOR s ty a)  => Show (Vector s ty a) where
  show v = "fromList " Prelude.++ showList (toList v) ""

-- | Internal wrapper type for reflection. First type parameter is the reified
-- type to reflect.
newtype W t ty s a = W { unW :: Vector s ty a }

withW :: proxy t -> Vector s ty a -> W t ty s a
withW _ v = W v

proxyFW :: (W t ty s a -> r) -> Vector s ty a -> p t -> r
proxyFW f v p = f (withW p v)

proxyFW2 :: (W t tya s a -> W t tyb s b -> r) -> Vector s tya a -> Vector s tyb b -> p t -> r
proxyFW2 f v1 v2 p = f (withW p v1) (withW p v2)

proxyW :: W t ty s a -> p t -> Vector s ty a
proxyW v _ = unW v

type instance G.Mutable (W t ty s) = Mutable.W t ty

instance (Reifies t (AcquireIO s), VECTOR s ty a) => G.Vector (W t ty s) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (Mutable.unW -> Mutable.MVector sx off len) = do
      fp <- foreignSEXP sx
      return $ W $ Vector fp off len
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (unW -> Vector fp off len) = unsafePrimToPrim $
      withForeignSEXP1 fp $ \ptr -> do
         sx' <- acquireIO (R.release ptr)
         return $ Mutable.withW p $ Mutable.MVector (R.unsafeRelease sx') off len
    where
      AcquireIO acquireIO = reflect (Proxy :: Proxy t)
      p = Proxy :: Proxy t
  basicLength (unW -> Vector _ _ len) = fromIntegral len
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice (fromIntegral ->i)
     (fromIntegral ->n) (unW -> Vector fp off _len) = W $ Vector fp (off + i) n
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM v i = return . unsafeInlineIO $ peekElemOff (unsafeToPtr (unW v)) i
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy mv v =
      unsafePrimToPrim $
        copyArray (Mutable.unsafeToPtr (Mutable.unW mv))
                  (unsafeToPtr (unW v))
                  (G.basicLength v)
  {-# INLINE elemseq #-}
  elemseq _ = seq

#if __GLASGOW_HASKELL__ >= 708
instance VECTOR s ty a => Exts.IsList (Vector s ty a) where
  type Item (Vector s ty a) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList
#endif

-- | Return Pointer of the first element of the vector storage.
unsafeToPtr :: Vector s ty a -> Ptr a
{-# INLINE unsafeToPtr #-}
unsafeToPtr v = unsafeInlineIO $ withForeignSEXP1 (vectorBase v) $ \p ->
   return $  (R.unsafeSEXPToVectorPtr p `plusPtr` (fromIntegral $ vectorOffset v))

-- | /O(n)/ Create an immutable vector from a 'SEXP'. Because 'SEXP's are
-- mutable, this function yields an immutable /copy/ of the 'SEXP'.
fromSEXP :: (VECTOR s ty a) => SEXP s ty -> Vector s ty a
fromSEXP s = phony $ \p -> runST $ do w <- run (proxyFW G.clone (unsafeFromSEXP s) p) 
                                      v <- G.unsafeFreeze w
                                      return (unW v)

-- | /O(1)/ Unsafe convert a mutable 'SEXP' to an immutable vector without
-- copying. The mutable vector must not be used after this operation, lest one
-- runs the risk of breaking referential transparency.
unsafeFromSEXP :: VECTOR s ty a
               => SEXP s ty
               -> Vector s ty a
unsafeFromSEXP s = unsafeInlineIO $ do
  sxp <- foreignSEXP s
  l <- R.length s
  return $ Vector sxp 0 (fromIntegral l)

-- | /O(n)/ Yield a (mutable) copy of the vector as a 'SEXP'.
toSEXP :: VECTOR s ty a => Vector s ty a -> SEXP s ty
toSEXP s = phony $ \p -> runST $ do w <- run (proxyFW G.clone s p)
                                    v <- G.unsafeFreeze w
                                    return (unsafeToSEXP (unW v))

-- | /O(1)/ Unsafely convert an immutable vector to a (mutable) 'SEXP' without
-- copying. The immutable vector must not be used after this operation.
unsafeToSEXP :: VECTOR s ty a => Vector s ty a -> SEXP s ty
unsafeToSEXP (Vector (ForeignSEXP fsx) _ _) = unsafePerformIO $ -- XXX
  withForeignPtr fsx $ return . R.sexp . castPtr

-- | /O(n)/ Convert a character vector into a 'String'.
toString :: Vector s 'Char Word8 -> String
toString v = unsafeInlineIO $
  GHC.peekCStringLen utf8 ( castPtr $ unsafeToPtr v
                          , fromIntegral $ vectorLength v)


-- | /O(n)/ Convert a character vector into a strict 'ByteString'.
toByteString :: Vector s 'Char Word8 -> ByteString
toByteString v = unsafeInlineIO $ 
   B.packCStringLen ( castPtr $ unsafeToPtr v
                    , fromIntegral $ vectorLength v)

------------------------------------------------------------------------
-- Vector API
--

------------------------------------------------------------------------
-- Length
------------------------------------------------------------------------

-- | /O(1)/ Yield the length of the vector.
length :: VECTOR s ty a => Vector s ty a -> Int
{-# INLINE length #-}
length v = phony $ proxyFW G.length v

-- | /O(1)/ Test whether a vector if empty
null :: VECTOR s ty a => Vector s ty a -> Bool
{-# INLINE null #-}
null v = phony $ proxyFW G.null v


------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(1) Indexing
(!) :: VECTOR s ty a => Vector s ty a -> Int -> a
{-# INLINE (!) #-}
(!) v i = phony $ proxyFW (G.! i) v

-- | O(1) Safe indexing
(!?) :: VECTOR s ty a => Vector s ty a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) v i = phony $ proxyFW (G.!? i) v

-- | /O(1)/ First element
head :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE head #-}
head v = phony $ proxyFW G.head v

-- | /O(1)/ Last element
last :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE last #-}
last v = phony $ proxyFW G.last v

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: VECTOR s ty a => Vector s ty a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex v i = phony $ proxyFW (`G.unsafeIndex` i) v

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE unsafeHead #-}
unsafeHead v = phony $ proxyFW G.unsafeHead v

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: VECTOR s ty a => Vector s ty a -> a
{-# INLINE unsafeLast #-}
unsafeLast v = phony $ proxyFW G.unsafeLast v

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
indexM v i = phony $ proxyFW (`G.indexM` i) v

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE headM #-}
headM v = phony $ proxyFW G.headM v

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE lastM #-}
lastM v = phony $ proxyFW G.lastM v

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (VECTOR s ty a, Monad m) => Vector s ty a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM v = phony $ proxyFW G.unsafeIndexM v

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM v = phony $ proxyFW G.unsafeHeadM v

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (VECTOR s ty a, Monad m) => Vector s ty a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM v = phony $ proxyFW G.unsafeLastM v

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
slice i n v = phony $ unW . proxyFW (G.slice i n) v

-- | /O(N)/ Yield all but the last element, this operation will copy an array.
-- The vector may not be empty.
init :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE init #-}
init v = phony $ unW . proxyFW G.init v

-- | /O(N)/ Copy all but the first element. The vector may not be empty.
tail :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE tail #-}
tail v = phony $ unW . proxyFW G.tail v

-- | /O(N)/ Yield at the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE take #-}
take i v = phony $ unW . proxyFW (G.take i) v

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE drop #-}
drop i v = phony $ unW . proxyFW (G.drop i) v

-- | /O(N)/ Yield the first @n@ elements paired with the remainder with copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: VECTOR s ty a => Int -> Vector s ty a -> (Vector s ty a, Vector s ty a)
splitAt i v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.splitAt i) v

-- | /O(N)/ Yield a slice of the vector with copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: VECTOR s ty a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector s ty a
                       -> Vector s ty a
{-# INLINE unsafeSlice #-}
unsafeSlice i j v = phony $ unW . proxyFW (G.unsafeSlice i j) v

-- | /O(N)/ Yield all but the last element with copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE unsafeInit #-}
unsafeInit v = phony $ unW . proxyFW G.unsafeInit v

-- | /O(N)/ Yield all but the first element with copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: VECTOR s ty a => Vector s ty a -> Vector s ty a
{-# INLINE unsafeTail #-}
unsafeTail v = phony $ unW . proxyFW G.unsafeTail v

-- | /O(N)/ Yield the first @n@ elements with copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE unsafeTake #-}
unsafeTake i v = phony $ unW . proxyFW (G.unsafeTake i) v

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: VECTOR s ty a => Int -> Vector s ty a -> Vector s ty a
{-# INLINE unsafeDrop #-}
unsafeDrop i v = phony $ unW . proxyFW (G.unsafeDrop i) v

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: VECTOR s ty a => Vector s ty a
{-# INLINE empty #-}
empty = phony $ proxyW G.empty

-- | /O(1)/ Vector with exactly one element
singleton :: VECTOR s ty a => a -> Vector s ty a
{-# INLINE singleton #-}
singleton a = phony $ proxyW (G.singleton a)

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: VECTOR s ty a => Int -> a -> Vector s ty a
{-# INLINE replicate #-}
replicate i v = phony $ proxyW (G.replicate i v)

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: VECTOR s ty a => Int -> (Int -> a) -> Vector s ty a
{-# INLINE generate #-}
generate i f = phony $ proxyW (G.generate i f)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: VECTOR s ty a => Int -> (a -> a) -> a -> Vector s ty a
{-# INLINE iterateN #-}
iterateN i f a = phony $ proxyW (G.iterateN i f a)

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
unfoldr g a = phony $ proxyW (G.unfoldr g a)

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: VECTOR s ty a => Int -> (b -> Maybe (a, b)) -> b -> Vector s ty a
{-# INLINE unfoldrN #-}
unfoldrN n g a = phony $ proxyW (G.unfoldrN n g a)

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: VECTOR s ty a => Int -> (Vector s ty a -> a) -> Vector s ty a
{-# INLINE constructN #-}
constructN n g = phony $ proxyW (G.constructN n (g.unW))

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: VECTOR s ty a => Int -> (Vector s ty a -> a) -> Vector s ty a
{-# INLINE constructrN #-}
constructrN n g = phony $ proxyW (G.constructrN n (g.unW))

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (VECTOR s ty a, Num a) => a -> Int -> Vector s ty a
{-# INLINE enumFromN #-}
enumFromN a i = phony $ proxyW (G.enumFromN a i)

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (VECTOR s ty a, Num a) => a -> a -> Int -> Vector s ty a
{-# INLINE enumFromStepN #-}
enumFromStepN f t s = phony $ proxyW (G.enumFromStepN f t s)

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (VECTOR s ty a, Enum a) => a -> a -> Vector s ty a
{-# INLINE enumFromTo #-}
enumFromTo f t = phony $ proxyW (G.enumFromTo f t)

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (VECTOR s ty a, Enum a) => a -> a -> a -> Vector s ty a
{-# INLINE enumFromThenTo #-}
enumFromThenTo f t s = phony $ proxyW (G.enumFromThenTo f t s)

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: VECTOR s ty a => a -> Vector s ty a -> Vector s ty a
{-# INLINE cons #-}
cons a v = phony $ unW . proxyFW (G.cons a) v

-- | /O(n)/ Append an element
snoc :: VECTOR s ty a => Vector s ty a -> a -> Vector s ty a
{-# INLINE snoc #-}
snoc v a = phony $ unW . proxyFW (`G.snoc` a) v

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: VECTOR s ty a => Vector s ty a -> Vector s ty a -> Vector s ty a
{-# INLINE (++) #-}
v1 ++ v2 = phony $ unW . proxyFW2 (G.++) v1 v2 

-- | /O(n)/ Concatenate all vectors in the list
concat :: VECTOR s ty a => [Vector s ty a] -> Vector s ty a
{-# INLINE concat #-}
concat vs = phony $ \p -> unW $ G.concat $ Prelude.map (withW p) vs

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, VECTOR s ty a) => Int -> m a -> m (Vector s ty a)
{-# INLINE replicateM #-}
replicateM n f = phony $ \p -> (\v -> proxyW v p) <$> G.replicateM n f

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, VECTOR s ty a) => Int -> (Int -> m a) -> m (Vector s ty a)
{-# INLINE generateM #-}
generateM n f = phony $ \p -> (\v -> proxyW v p) <$> G.generateM n f

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: VECTOR s ty a => (forall r. ST r (MVector r ty a)) -> Vector s ty a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create f = phony $ \p -> unW $ G.create (Mutable.withW p <$> f)

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
force v = phony $ unW . proxyFW G.force v

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: VECTOR s ty a
     => Vector s ty a   -- ^ initial vector (of length @m@)
     -> [(Int, a)]      -- ^ list of index/value pairs (of length @n@)
     -> Vector s ty a
{-# INLINE (//) #-}
(//) v l = phony $ unW . proxyFW (G.// l) v

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

-- | Same as ('//') but without bounds checking.
unsafeUpd :: VECTOR s ty a => Vector s ty a -> [(Int, a)] -> Vector s ty a
{-# INLINE unsafeUpd #-}
unsafeUpd v l = phony $ unW . proxyFW (`G.unsafeUpd` l) v

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
accum f v l = phony $ unW . proxyFW (\w -> G.accum f w l) v

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
unsafeAccum f v l = phony $ unW . proxyFW (\w -> G.unsafeAccum f w l) v

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
reverse v = phony $ unW . proxyFW G.reverse v

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
map f v = phony $ unW . proxyFW (G.map f) v

-- | /O(n)/ Apply a function to every element of a Vector s ty and its index
imap :: (VECTOR s ty a, VECTOR s ty b) => (Int -> a -> b) -> Vector s ty a -> Vector s ty b
{-# INLINE imap #-}
imap f v = phony $ unW . proxyFW (G.imap f) v


-- | Map a function over a Vector s ty and concatenate the results.
concatMap :: (VECTOR s tya a, VECTOR s tyb b)
          => (a -> Vector s tyb b)
          -> Vector s tya a
          -> Vector s tyb b
{-# INLINE concatMap #-}
concatMap f v =
    phony $ \p ->
    (`proxyW` p) $
    G.unstream $
    Stream.concatMap (G.stream . withW p . f) $
    G.stream $
    withW p v

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, VECTOR s ty a, VECTOR s ty b) => (a -> m b) -> Vector s ty a -> m (Vector s ty b)
{-# INLINE mapM #-}
mapM f v = phony $ \p -> unW <$> proxyFW (G.mapM f) v p

-- | /O(n)/ Apply the monadic action to all elements of a Vector s ty and ignore the
-- results
mapM_ :: (Monad m, VECTOR s ty a) => (a -> m b) -> Vector s ty a -> m ()
{-# INLINE mapM_ #-}
mapM_ f v = phony $ proxyFW (G.mapM_ f) v

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, VECTOR s ty a, VECTOR s ty b) => Vector s ty a -> (a -> m b) -> m (Vector s ty b)
{-# INLINE forM #-}
forM v f = phony $ \p -> unW <$> proxyFW (`G.forM` f) v p

-- | /O(n)/ Apply the monadic action to all elements of a Vector s ty and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, VECTOR s ty a) => Vector s ty a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ v f = phony $ proxyFW (`G.forM_` f) v

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
        => (a -> b -> c) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c
{-# INLINE zipWith #-}
zipWith f xs ys = phony $ \p ->
   proxyW (G.unstream (Stream.zipWith f (G.stream (withW p xs)) (G.stream (withW p ys)))) p

-- | Zip three vectors with the given function.
zipWith3 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d)
         => (a -> b -> c -> d) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith3 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)))) p

zipWith4 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e)
         => (a -> b -> c -> d -> e)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith4 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)))) p

zipWith5 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
             VECTOR s tyf f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
         -> Vector s tyf f
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith5 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)))) p

zipWith6 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
             VECTOR s tyf f, VECTOR s tyg g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
         -> Vector s tyf f -> Vector s tyg g
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith6 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)) (G.stream (withW p fs)))) p

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
         => (Int -> a -> b -> c) -> Vector s tya a -> Vector s tyb b -> Vector s tyc c
{-# INLINE izipWith #-}
izipWith f as bs = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)))) p

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d)
          => (Int -> a -> b -> c -> d)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)))) p

izipWith4 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)))) p

izipWith5 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
              VECTOR s tyf f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
          -> Vector s tyf f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)))) p

izipWith6 :: (VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c, VECTOR s tyd d, VECTOR s tye e,
              VECTOR s tyf f, VECTOR s tyg g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector s tya a -> Vector s tyb b -> Vector s tyc c -> Vector s tyd d -> Vector s tye e
          -> Vector s tyf f -> Vector s tyg g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs = phony $ \p ->
  proxyW (G.unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)) (G.stream (withW p fs)))) p

-- Monadic zipping
-- ---------------


-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, VECTOR s tya a, VECTOR s tyb b, VECTOR s tyc c)
         => (a -> b -> m c)
         -> Vector s tya a
         -> Vector s tyb b
         -> m (Vector s tyc c)
{-# INLINE zipWithM #-}
zipWithM f xs ys = phony $ \p ->
    proxyW <$>
    unstreamM (Stream.zipWithM f (G.stream (withW p xs)) (G.stream (withW p ys))) <*>
    return p
  where
    -- Inlined from vector-0.10, which doesn't export unstreamM.
    unstreamM s = do
        zs <- MStream.toList s
        return $ G.unstream $ Stream.unsafeFromList (MStream.size s) zs


-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, VECTOR s tya a, VECTOR s tyb b)
          => (a -> b -> m c)
          -> Vector s tya a
          -> Vector s tyb b
          -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys = phony $ \p ->
    Stream.zipWithM_ f (G.stream (withW p xs)) (G.stream (withW p ys))

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE filter #-}
filter f v = phony $ unW . proxyFW (G.filter f) v

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: VECTOR s ty a => (Int -> a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE ifilter #-}
ifilter f v = phony $ unW . proxyFW (G.ifilter f) v

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, VECTOR s ty a) => (a -> m Bool) -> Vector s ty a -> m (Vector s ty a)
{-# INLINE filterM #-}
filterM f v = phony $ \p -> unW <$> proxyFW (G.filterM f) v p

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- with copying.
takeWhile :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE takeWhile #-}
takeWhile f v = phony $ unW . proxyFW (G.takeWhile f) v

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- with copying.
dropWhile :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector s ty a
{-# INLINE dropWhile #-}
dropWhile f v = phony $ unW . proxyFW (G.dropWhile f) v

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE partition #-}
partition f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.partition f) v

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE unstablePartition #-}
unstablePartition f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.unstablePartition f) v

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest with copying.
span :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE span #-}
span f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.span f) v

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest with copying.
break :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> (Vector s ty a, Vector s ty a)
{-# INLINE break #-}
break f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.break f) v

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Bool
{-# INLINE elem #-}
elem a v = phony $ proxyFW (G.elem a) v

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Bool
{-# INLINE notElem #-}
notElem a v = phony $ proxyFW (G.notElem a) v

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Maybe a
{-# INLINE find #-}
find f v = phony $ proxyFW (G.find f) v

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f v = phony $ proxyFW (G.findIndex f) v

{-
-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Vector Int
{-# INLINE findIndices #-}
findIndices f v = phony $ proxyFW (G.findIndices f) v
-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex a v = phony $ proxyFW (G.elemIndex a) v

{-
-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (VECTOR s ty a, Eq a) => a -> Vector s ty a -> Vector s 'R.Int Int32
{-# INLINE elemIndices #-}
elemIndices s v = phony $ unW . proxyFW (G.elemIndices s) v
-}

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: VECTOR s ty b => (a -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE foldl #-}
foldl f s v = phony $ proxyFW (G.foldl f s) v

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldl1 #-}
foldl1 f v = phony $ proxyFW (G.foldl1 f) v

-- | /O(n)/ Left fold with strict accumulator
foldl' :: VECTOR s ty b => (a -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE foldl' #-}
foldl' f s v = phony $ proxyFW (G.foldl' f s) v

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldl1' #-}
foldl1' f v  = phony $ proxyFW (G.foldl1' f) v

-- | /O(n)/ Right fold
foldr :: VECTOR s ty a => (a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE foldr #-}
foldr f s v = phony $ proxyFW (G.foldr f s) v

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldr1 #-}
foldr1 f v = phony $ proxyFW (G.foldr1 f) v

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: VECTOR s ty a => (a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE foldr' #-}
foldr' f s v = phony $ proxyFW (G.foldr' f s) v

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> a
{-# INLINE foldr1' #-}
foldr1' f v = phony $ proxyFW (G.foldr1' f) v

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: VECTOR s ty b => (a -> Int -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE ifoldl #-}
ifoldl f s v = phony $ proxyFW (G.ifoldl f s) v

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: VECTOR s ty b => (a -> Int -> b -> a) -> a -> Vector s ty b -> a
{-# INLINE ifoldl' #-}
ifoldl' f s  v = phony $ proxyFW (G.ifoldl' f s) v

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: VECTOR s ty a => (Int -> a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE ifoldr #-}
ifoldr f s v = phony $ proxyFW (G.ifoldr f s) v

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: VECTOR s ty a => (Int -> a -> b -> b) -> b -> Vector s ty a -> b
{-# INLINE ifoldr' #-}
ifoldr' f s v = phony $ proxyFW (G.ifoldr' f s) v

-- Specialised folds
-- -----------------


-- | /O(n)/ Check if all elements satisfy the predicate.
all :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Bool
{-# INLINE all #-}
all f v = phony $ \p -> G.all f (withW p v)

-- | /O(n)/ Check if any element satisfies the predicate.
any :: VECTOR s ty a => (a -> Bool) -> Vector s ty a -> Bool
{-# INLINE any #-}
any f v = phony $ \p -> G.any f (withW p v)

-- -- | /O(n)/ Check if all elements are 'True'
-- and :: Vector s 'Logical Bool -> Bool
-- {-# INLINE and #-}
-- and v = phony $ \p -> G.and (withW p v)
--
-- -- | /O(n)/ Check if any element is 'True'
-- or :: Vector s 'Logical Bool -> Bool
-- {-# INLINE or #-}
-- or v = phony $ \p -> G.or (withW p v)

-- | /O(n)/ Compute the sum of the elements
sum :: (VECTOR s ty a, Num a) => Vector s ty a -> a
{-# INLINE sum #-}
sum v = phony $ proxyFW G.sum v

-- | /O(n)/ Compute the produce of the elements
product :: (VECTOR s ty a, Num a) => Vector s ty a -> a
{-# INLINE product #-}
product v = phony $ proxyFW G.product v

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (VECTOR s ty a, Ord a) => Vector s ty a -> a
{-# INLINE maximum #-}
maximum v = phony $ proxyFW G.maximum v

-- | /O(n)/ Yield the maximum element of the Vector s ty according to the given
-- comparison function. The vector may not be empty.
maximumBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> a
{-# INLINE maximumBy #-}
maximumBy f v = phony $ proxyFW (G.maximumBy f) v

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (VECTOR s ty a, Ord a) => Vector s ty a -> a
{-# INLINE minimum #-}
minimum v = phony $ proxyFW G.minimum v

-- | /O(n)/ Yield the minimum element of the Vector s ty according to the given
-- comparison function. The vector may not be empty.
minimumBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> a
{-# INLINE minimumBy #-}
minimumBy f v = phony $ proxyFW (G.minimumBy f) v

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (VECTOR s ty a, Ord a) => Vector s ty a -> Int
{-# INLINE maxIndex #-}
maxIndex v = phony $ proxyFW G.maxIndex v

-- | /O(n)/ Yield the index of the maximum element of the Vector s ty according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy f v = phony $ proxyFW (G.maxIndexBy f) v

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (VECTOR s ty a, Ord a) => Vector s ty a -> Int
{-# INLINE minIndex #-}
minIndex v = phony $ proxyFW G.minIndex v

-- | /O(n)/ Yield the index of the minimum element of the Vector s ty according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: VECTOR s ty a => (a -> a -> Ordering) -> Vector s ty a -> Int
{-# INLINE minIndexBy #-}
minIndexBy f v = phony $ proxyFW (G.minIndexBy f) v

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m a
{-# INLINE foldM #-}
foldM f s v = phony $ proxyFW (G.foldM f s) v

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m a
{-# INLINE fold1M #-}
fold1M f v = phony $ proxyFW (G.fold1M f) v

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m a
{-# INLINE foldM' #-}
foldM' f s v = phony $ proxyFW (G.foldM' f s) v

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m a
{-# INLINE fold1M' #-}
fold1M' f v = phony $ proxyFW (G.fold1M' f) v

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m ()
{-# INLINE foldM_ #-}
foldM_ f s v = phony $ proxyFW (G.foldM_ f s) v

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ f v = phony $ proxyFW (G.fold1M_ f) v

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, VECTOR s ty b) => (a -> b -> m a) -> a -> Vector s ty b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ f s v = phony $ proxyFW (G.foldM'_ f s) v

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, VECTOR s ty a) => (a -> a -> m a) -> Vector s ty a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ f v = phony $ proxyFW (G.fold1M'_ f) v

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
prescanl f s v = phony $ unW . proxyFW (G.prescanl f s) v

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE prescanl' #-}
prescanl' f s v = phony $ unW . proxyFW (G.prescanl' f s) v

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
postscanl f s v = phony $ unW . proxyFW (G.postscanl f s) v

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE postscanl' #-}
postscanl' f s v = phony $ unW . proxyFW (G.postscanl' f s) v

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
scanl f s v = phony $ unW . proxyFW (G.scanl f s) v

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> a) -> a -> Vector s ty b -> Vector s ty a
{-# INLINE scanl' #-}
scanl' f s v = phony $ unW . proxyFW (G.scanl' f s) v

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanl1 #-}
scanl1 f v = phony $ unW . proxyFW (G.scanl1 f) v

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanl1' #-}
scanl1' f v = phony $ unW . proxyFW (G.scanl1' f) v

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE prescanr #-}
prescanr f s v = phony $ unW . proxyFW (G.prescanr f s) v

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE prescanr' #-}
prescanr' f s v = phony $ unW . proxyFW (G.prescanr' f s) v

-- | /O(n)/ Right-to-left scan
postscanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE postscanr #-}
postscanr f s v = phony $ unW . proxyFW (G.postscanr f s) v

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE postscanr' #-}
postscanr' f s v = phony $ unW . proxyFW (G.postscanr' f s) v

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE scanr #-}
scanr f s v = phony $ unW . proxyFW (G.scanr f s) v

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (VECTOR s ty a, VECTOR s ty b) => (a -> b -> b) -> b -> Vector s ty a -> Vector s ty b
{-# INLINE scanr' #-}
scanr' f s v = phony $ unW . proxyFW (G.scanr' f s) v

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanr1 #-}
scanr1 f v = phony $ unW . proxyFW (G.scanr1 f) v

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: VECTOR s ty a => (a -> a -> a) -> Vector s ty a -> Vector s ty a
{-# INLINE scanr1' #-}
scanr1' f v = phony $ unW . proxyFW (G.scanr1' f) v

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: VECTOR s ty a => Vector s ty a -> [a]
{-# INLINE toList #-}
toList v = phony $ proxyFW G.toList v

-- | /O(n)/ Convert a list to a vector
fromList :: forall s ty a . VECTOR s ty a => [a] -> Vector s ty a
{-# INLINE fromList #-}
fromList xs = phony $ proxyW (G.fromListN (Prelude.length xs) xs)

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: forall s ty a . VECTOR s ty a => Int -> [a] -> Vector s ty a
{-# INLINE fromListN #-}
fromListN i l = phony $ proxyW (G.fromListN i l)

-- Conversions - Unsafe casts
-- --------------------------

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one with
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: (VECTOR (Region m) ty a, MonadR m)
             => MVector (Region m) ty a -> m (Vector (Region m) ty a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze m = withAcquire $ \p -> unW <$> G.unsafeFreeze (Mutable.withW p m)

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one with
-- copying. The immutable vector may not be used after this operation.
unsafeThaw :: (MonadR m, VECTOR (Region m) ty a)
           => Vector (Region m) ty a -> m (MVector (Region m) ty a)
{-# INLINE unsafeThaw #-}
unsafeThaw v = withAcquire $ \p -> Mutable.unW <$> G.unsafeThaw (withW p v)

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (MonadR m, VECTOR (Region m) ty a)
     => Vector (Region m) ty a -> m (MVector (Region m) ty a)
{-# INLINE thaw #-}
thaw v1 = withAcquire $ \p -> Mutable.unW <$> G.thaw (withW p v1)

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (MonadR m, VECTOR (Region m) ty a)
       => MVector (Region m) ty a -> m (Vector (Region m) ty a)
{-# INLINE freeze #-}
freeze m1 = withAcquire $ \p -> unW <$> G.freeze (Mutable.withW p m1)

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (MonadR m, VECTOR (Region m) ty a)
  => MVector (Region m) ty a -> Vector (Region m) ty a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy m1 v2 = withAcquire $ \p -> G.unsafeCopy (Mutable.withW p m1) (withW p v2)

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a -> Vector (Region m) ty a -> m ()
{-# INLINE copy #-}
copy m1 v2 = withAcquire $ \p -> G.copy (Mutable.withW p m1) (withW p v2)

phony :: (forall t . Reifies t (AcquireIO s) => Proxy t -> r) -> r
phony f = reify (AcquireIO acquireIO) $ \p ->  f p
  where
    acquireIO :: SEXP V ty -> IO (SEXP g ty)
    acquireIO x = do
      R.preserveObject x
      return $ R.unsafeRelease x
