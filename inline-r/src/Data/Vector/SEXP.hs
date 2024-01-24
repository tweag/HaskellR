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

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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

{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--prune-unsorted" @-}
module Data.Vector.SEXP
  ( Vector(..)
  , Mutable.MVector(..)
  , ElemRep
  , VSEXPTYPE(..)
  , Data.Vector.SEXP.fromSEXP
  , unsafeFromSEXP
  , Data.Vector.SEXP.toSEXP
  , unsafeToSEXP
  , vtypeOf
  , vstypeOf
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
  , unsafeWithByteString
  ) where

import Control.Exception (evaluate)
import Control.Monad.R.Class
import Control.Monad.R.Internal
import Control.Memory.Region
import Data.Vector.SEXP.Base
import Data.Vector.SEXP.Mutable (MVector)
import qualified Data.Vector.SEXP.Mutable as Mutable
import qualified Data.Vector.SEXP.Mutable.Internal as Mutable
import Foreign.R ( SEXP(..), SEXP0(..) )
import qualified Foreign.R as R

import Control.Monad.ST (ST, runST)
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..), reify)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.New (run)
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import Control.Applicative hiding (empty)
import Control.Exception (mask_)
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle.Monadic as Bundle
import           Data.Vector.Fusion.Bundle.Monadic (sSize, sElems)
import           Data.Vector.Fusion.Bundle.Size (Size(Unknown), smaller)
import           Data.Vector.Fusion.Bundle (lift)
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.List as List
#else
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
#endif

import Control.Monad.Primitive ( PrimMonad, unsafeInlineIO, unsafePrimToPrim )
import qualified Control.DeepSeq as DeepSeq
import Data.Word ( Word8 )
import Foreign ( Storable, Ptr, castPtr, peekElemOff )
import Foreign.C.String -- XXX: Needed for LH name resolution
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Array ( copyArray )
import qualified GHC.Foreign as GHC
import qualified GHC.ForeignPtr as GHC
import GHC.IO.Encoding.UTF8
import Internal.Error (violation)
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

newtype ForeignSEXP = ForeignSEXP (ForeignPtr R.SEXPREC)

{-@ measure Data.Vector.SEXP.ftypeOf :: ForeignSEXP -> R.SEXPTYPE @-}

-- | Create a 'ForeignSEXP' from 'SEXP'.
{-@ assume foreignSEXP :: s:SEXP s -> m {v:ForeignSEXP | Data.Vector.SEXP.ftypeOf v == typeOf s} @-}
foreignSEXP :: PrimMonad m => SEXP s -> m ForeignSEXP
foreignSEXP sx@(SEXP (SEXP0 ptr)) =
    unsafePrimToPrim $ mask_ $ do
      R.preserveObject sx
      ForeignSEXP <$> GHC.newConcForeignPtr ptr (R.releaseObject sx)

{-@ assume withForeignSEXP :: f:ForeignSEXP -> (TSEXP V (Data.Vector.SEXP.ftypeOf f) -> IO r) -> IO r @-}
withForeignSEXP :: ForeignSEXP -> (SEXP V -> IO r) -> IO r
withForeignSEXP (ForeignSEXP fptr) f =
    withForeignPtr fptr $ \ptr -> f (SEXP (SEXP0 ptr))

{-@ measure Data.Vector.SEXP.vtypeOf :: Vector a -> R.SEXPTYPE @-}
{-@ type TVector a T = {v:Vector a | Data.Vector.SEXP.vtypeOf v == T} @-}

-- | Immutable vectors.
data Vector a = Vector
  { vectorBase   :: ForeignSEXP
  , vectorOffset :: Int32
  , vectorLength :: Int32
  }

{-@ assume vtypeOf :: v:Vector a -> {t:R.SEXPTYPE | Data.Vector.SEXP.vtypeOf v == t} @-}
vtypeOf :: Vector a -> R.SEXPTYPE
vtypeOf v = unsafeInlineIO $ withForeignSEXP (vectorBase v) $ pure . R.typeOf

instance (Eq a, Storable a) => Eq (Vector a) where
  a == b = toList a == toList b

instance (Show a, Storable a)  => Show (Vector a) where
  show v = "fromList " Prelude.++ showList (toList v) ""

-- | Internal wrapper type for reflection. First type parameter is the reified
-- type to reflect.
newtype W t a = W { unW :: Vector a }

withW :: proxy t -> Vector a -> W t a
withW _ v = W v

proxyFW :: (W t a -> r) -> Vector a -> p t -> r
proxyFW f v p = f (withW p v)

proxyFW2 :: (W t a -> W t b -> r) -> Vector a -> Vector b -> p t -> r
proxyFW2 f v1 v2 p = f (withW p v1) (withW p v2)

proxyW :: W t a -> p t -> Vector a
proxyW v _ = unW v

type instance G.Mutable (W t) = Mutable.W t

instance (Reifies t (AcquireIO s, IO R.SEXPTYPE), Storable a) => G.Vector (W t) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (Mutable.unW -> Mutable.MVector sx off len) = do
      fp <- foreignSEXP sx
      return $ W $ Vector fp off len
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (unW -> Vector fp off len) = unsafePrimToPrim $
      withForeignSEXP fp $ \ptr -> do
         sx' <- acquireIO (R.release ptr)
         return $ Mutable.withW p $ Mutable.MVector (R.unsafeRelease sx') off len
    where
      (acquireIO, _) = reflect (Proxy :: Proxy t)
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

-- TODO: how to disable LH on a class instance?
-- instance Storable a => Exts.IsList (Vector a) where
--   type Item (Vector a) = a
  -- TODO: How to disable LH so it doesn't complain of these methods calling error
  -- fromList = Prelude.error "cannot convert list to vector without an explicit runtime type"
  -- fromListN = Prelude.error "cannot convert list to vector without an explicit runtime type"
  -- fromList = fromList
  -- fromListN = fromListN
--  toList = toList

-- | Return Pointer of the first element of the vector storage.
unsafeToPtr :: Storable a => Vector a -> Ptr a
{-# INLINE unsafeToPtr #-}
unsafeToPtr (Vector fp off len) = unsafeInlineIO $ withForeignSEXP fp $ \sx ->
    return $ Mutable.unsafeToPtr $ Mutable.MVector sx off len

-- | /O(n)/ Create an immutable vector from a 'SEXP'. Because 'SEXP's are
-- mutable, this function yields an immutable /copy/ of the 'SEXP'.
{-@ assume fromSEXP :: {s:SEXP s | isVectorType (typeOf s)} -> {v:Vector a | Data.Vector.SEXP.vtypeOf v == typeOf s} @-}
{-@ ignore fromSEXP @-}
fromSEXP :: Storable a => SEXP s -> Vector a
fromSEXP s = phony $ \p -> runST $ do
  w <- run (proxyFW G.clone (unsafeFromSEXP s) p)
  v <- G.unsafeFreeze w
  return (unW v)

-- | /O(1)/ Unsafe convert a mutable 'SEXP' to an immutable vector without
-- copying. The mutable vector must not be used after this operation, lest one
-- runs the risk of breaking referential transparency.
{-@ assume unsafeFromSEXP :: {s:SEXP s | isVectorType (typeOf s)} -> {v:Vector a | Data.Vector.SEXP.vtypeOf v == typeOf s} @-}
unsafeFromSEXP :: Storable a
               => SEXP s
               -> Vector a
unsafeFromSEXP s = unsafeInlineIO $ do
  sxp <- foreignSEXP s
  l <- R.length s
  return $ Vector sxp 0 (fromIntegral l)

-- | /O(n)/ Yield a (mutable) copy of the vector as a 'SEXP'.

{-@ assume toSEXP :: v:Vector a -> {s:SEXP s | typeOf s == Data.Vector.SEXP.vtypeOf v} @-}
toSEXP :: Storable a => Vector a -> SEXP s
toSEXP s = phony $ \p -> runST $ do
  w <- run (proxyFW G.clone s p)
  v <- G.unsafeFreeze w
  return (unsafeToSEXP (unW v))

-- | /O(1)/ Unsafely convert an immutable vector to a (mutable) 'SEXP' without
-- copying. The immutable vector must not be used after this operation.
{-@ assume unsafeToSEXP :: v:Vector a -> {s:SEXP s | typeOf s == Data.Vector.SEXP.vtypeOf v} @-}
unsafeToSEXP :: Storable a => Vector a -> SEXP s
unsafeToSEXP (Vector (ForeignSEXP fsx) _ _) = unsafePerformIO $ -- XXX
  withForeignPtr fsx $ return . R.sexp . SEXP0

-- | /O(n)/ Convert a character vector into a 'String'.
{-@ assume toString :: TVector Word8 Foreign.R.Type.SChar -> String @-}
{-@ ignore toString @-}
toString :: Vector Word8 -> String
toString v = unsafeInlineIO $
  GHC.peekCStringLen utf8 ( castPtr $ unsafeToPtr v
                          , fromIntegral $ vectorLength v)

-- | /O(n)/ Convert a character vector into a strict 'ByteString'.
{-@ assume toByteString :: TVector Word8 Foreign.R.Type.SChar -> ByteString @-}
{-@ ignore toByteString @-}
toByteString :: Vector Word8 -> ByteString
toByteString v = unsafeInlineIO $
   B.packCStringLen ( castPtr $ unsafeToPtr v
                    , fromIntegral $ vectorLength v)

-- | This function is unsafe and ByteString should not be used
-- outside of the function. Any change to bytestring will be
-- reflected in the source vector, thus breaking referencial
-- transparancy.
{-@ assume unsafeWithByteString :: TVector Word8 Foreign.R.Type.SChar -> (ByteString -> IO a) -> a @-}
{-@ ignore unsafeWithByteString @-}
unsafeWithByteString :: DeepSeq.NFData a => Vector Word8 -> (ByteString -> IO a) -> a
unsafeWithByteString v f = unsafeInlineIO $ do
   x <- B.unsafePackCStringLen (castPtr $ unsafeToPtr v
                               ,fromIntegral $ vectorLength v)
   w <- DeepSeq.force <$> f x
   evaluate w

------------------------------------------------------------------------
-- Vector API
--

------------------------------------------------------------------------
-- Length
------------------------------------------------------------------------

-- | /O(1)/ Yield the length of the vector.
length :: Storable a => Vector a -> Int
{-# INLINE length #-}
length v = phony $ proxyFW G.length v

-- | /O(1)/ Test whether a vector if empty
null :: Storable a => Vector a -> Bool
{-# INLINE null #-}
null v = phony $ proxyFW G.null v

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(1) Indexing
(!) :: Storable a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) v i = phony $ proxyFW (G.! i) v

-- | O(1) Safe indexing
(!?) :: Storable a => Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) v i = phony $ proxyFW (G.!? i) v

-- | /O(1)/ First element
head :: Storable a => Vector a -> a
{-# INLINE head #-}
head v = phony $ proxyFW G.head v

-- | /O(1)/ Last element
last :: Storable a => Vector a -> a
{-# INLINE last #-}
last v = phony $ proxyFW G.last v

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: Storable a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex v i = phony $ proxyFW (`G.unsafeIndex` i) v

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: Storable a => Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead v = phony $ proxyFW G.unsafeHead v

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: Storable a => Vector a -> a
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
indexM :: (Storable a, Monad m) => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM v i = phony $ proxyFW (`G.indexM` i) v

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE headM #-}
headM v = phony $ proxyFW G.headM v

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE lastM #-}
lastM v = phony $ proxyFW G.lastM v

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Storable a, Monad m) => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM v = phony $ proxyFW G.unsafeIndexM v

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM v = phony $ proxyFW G.unsafeHeadM v

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM v = phony $ proxyFW G.unsafeLastM v

------------------------------------------------------------------------
-- Extracting subvectors (slicing)
------------------------------------------------------------------------

-- | /O(N)/ Yield a slice of the vector with copying it. The vector must
-- contain at least @i+n@ elements.
{-@ assume slice :: Int -> Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
slice :: Storable a
      => Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice i n v = phony $ unW . proxyFW (G.slice i n) v

-- | /O(N)/ Yield all but the last element, this operation will copy an array.
-- The vector may not be empty.
{-@ assume init :: v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
init :: Storable a => Vector a -> Vector a
{-# INLINE init #-}
init v = phony $ unW . proxyFW G.init v

-- | /O(N)/ Copy all but the first element. The vector may not be empty.
{-@ assume tail :: v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
tail :: Storable a => Vector a -> Vector a
{-# INLINE tail #-}
tail v = phony $ unW . proxyFW G.tail v

-- | /O(N)/ Yield at the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
{-@ assume take :: Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
take :: Storable a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take i v = phony $ unW . proxyFW (G.take i) v

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
{-@ assume drop :: Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
drop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop i v = phony $ unW . proxyFW (G.drop i) v

-- | /O(N)/ Yield the first @n@ elements paired with the remainder with copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-@ assume splitAt :: Int -> v:Vector a -> (TVector a (Data.Vector.SEXP.vtypeOf v), TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
{-# INLINE splitAt #-}
splitAt :: Storable a => Int -> Vector a -> (Vector a, Vector a)
splitAt i v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.splitAt i) v

-- | /O(N)/ Yield a slice of the vector with copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
{-@ assume unsafeSlice :: Int -> Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
unsafeSlice :: Storable a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice i j v = phony $ unW . proxyFW (G.unsafeSlice i j) v

-- | /O(N)/ Yield all but the last element with copying. The vector may not
-- be empty but this is not checked.
{-@ assume unsafeInit :: v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
unsafeInit :: Storable a => Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit v = phony $ unW . proxyFW G.unsafeInit v

-- | /O(N)/ Yield all but the first element with copying. The vector may not
-- be empty but this is not checked.
{-@ assume unsafeTail :: v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
unsafeTail :: Storable a => Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail v = phony $ unW . proxyFW G.unsafeTail v

-- | /O(N)/ Yield the first @n@ elements with copying. The vector must
-- contain at least @n@ elements but this is not checked.
{-@ assume unsafeTake :: Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
unsafeTake :: Storable a => Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake i v = phony $ unW . proxyFW (G.unsafeTake i) v

-- | /O(N)/ Yield all but the first @n@ elements with copying. The vector
-- must contain at least @n@ elements but this is not checked.
{-@ assume unsafeDrop :: Int -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
unsafeDrop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop i v = phony $ unW . proxyFW (G.unsafeDrop i) v

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
{-@ assume empty :: x:VSEXPTYPE s a -> TVector a (vstypeOf x) @-}
empty :: Storable a => VSEXPTYPE s a -> Vector a
{-# INLINE empty #-}
empty t = phonyvtype t $ proxyW G.empty

-- | /O(1)/ Vector with exactly one element
{-@ assume singleton :: x:VSEXPTYPE s a -> a -> TVector a (vstypeOf x) @-}
singleton :: Storable a => VSEXPTYPE s a -> a -> Vector a
{-# INLINE singleton #-}
singleton t a = phonyvtype t $ proxyW (G.singleton a)

-- | /O(n)/ Vector of the given length with the same value in each position
{-@ assume replicate :: x:VSEXPTYPE s a -> Int -> a -> TVector a (vstypeOf x) @-}
replicate :: Storable a => VSEXPTYPE s a -> Int -> a -> Vector a
{-# INLINE replicate #-}
replicate t i v = phonyvtype t $ proxyW (G.replicate i v)

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
{-@ assume generate :: x:VSEXPTYPE s a -> Int -> (Int -> a) -> TVector a (vstypeOf x) @-}
generate :: Storable a => VSEXPTYPE s a -> Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate t i f = phonyvtype t $ proxyW (G.generate i f)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
{-@ assume iterateN :: x:VSEXPTYPE s a -> Int -> (a -> a) -> a -> TVector a (vstypeOf x) @-}
iterateN :: Storable a => VSEXPTYPE s a -> Int -> (a -> a) -> a -> Vector a
{-# INLINE iterateN #-}
iterateN t i f a = phonyvtype t $ proxyW (G.iterateN i f a)

-- Unfolding
-- ---------
-- | /O(n)/ Construct a Vector ty by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
{-@ assume unfoldr :: x:VSEXPTYPE s a -> (b -> Maybe (a, b)) -> b -> TVector a (vstypeOf x) @-}
unfoldr :: Storable a => VSEXPTYPE s a -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr t g a = phonyvtype t $ proxyW (G.unfoldr g a)

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
{-@ assume unfoldrN :: x:VSEXPTYPE s a -> Int -> (b -> Maybe (a, b)) -> b -> TVector a (vstypeOf x) @-}
unfoldrN :: Storable a => VSEXPTYPE s a -> Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN t n g a = phonyvtype t $ proxyW (G.unfoldrN n g a)

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
{-@ assume constructN :: x:VSEXPTYPE s a -> Int -> (TVector a (vstypeOf x) -> a) -> TVector a (vstypeOf x) @-}
constructN :: Storable a => VSEXPTYPE s a -> Int -> (Vector a -> a) -> Vector a
{-# INLINE constructN #-}
constructN t n g = phonyvtype t $ proxyW (G.constructN n (g.unW))

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
{-@ assume constructrN :: x:VSEXPTYPE s a -> Int -> (TVector a (vstypeOf x) -> a) -> TVector a (vstypeOf x) @-}
constructrN :: Storable a => VSEXPTYPE s a -> Int -> (Vector a -> a) -> Vector a
{-# INLINE constructrN #-}
constructrN t n g = phonyvtype t $ proxyW (G.constructrN n (g.unW))

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
{-@ assume enumFromN :: x:VSEXPTYPE s a -> a -> Int -> TVector a (vstypeOf x) @-}
enumFromN :: (Storable a, Num a) => VSEXPTYPE s a -> a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN t a i = phonyvtype t $ proxyW (G.enumFromN a i)

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
{-@ assume enumFromStepN :: x:VSEXPTYPE s a -> a -> a -> Int -> TVector a (vstypeOf x) @-}
enumFromStepN :: (Storable a, Num a) => VSEXPTYPE s a -> a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN vt f t s = phonyvtype vt $ proxyW (G.enumFromStepN f t s)

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
{-@ assume enumFromTo :: x:VSEXPTYPE s a -> a -> a -> TVector a (vstypeOf x) @-}
enumFromTo :: (Storable a, Enum a) => VSEXPTYPE s a -> a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo vt f t = phonyvtype vt $ proxyW (G.enumFromTo f t)

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
{-@ assume enumFromThenTo :: x:VSEXPTYPE s a -> a -> a -> a -> TVector a (vstypeOf x) @-}
enumFromThenTo :: (Storable a, Enum a) => VSEXPTYPE s a -> a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo vt f t s = phonyvtype vt $ proxyW (G.enumFromThenTo f t s)

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
{-@ assume cons :: a -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
cons :: Storable a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons a v = phony $ unW . proxyFW (G.cons a) v

-- | /O(n)/ Append an element
{-@ assume snoc :: v:Vector a -> a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
snoc :: Storable a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc v a = phony $ unW . proxyFW (`G.snoc` a) v

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
{-@ assume (++) :: x:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf x) -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
(++) :: Storable a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
v1 ++ v2 = phony $ unW . proxyFW2 (G.++) v1 v2

-- | /O(n)/ Concatenate all vectors in the list
{-@ assume concat :: x:VSEXPTYPE s a -> [TVector a (vstypeOf x)] -> TVector a (vstypeOf x) @-}
concat :: Storable a => VSEXPTYPE s a -> [Vector a] -> Vector a
{-# INLINE concat #-}
concat t vs = phonyvtype t $ \p -> unW $ G.concat $ Prelude.map (withW p) vs

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
{-@ assume replicateM :: x:VSEXPTYPE s a -> Int -> m a -> m (TVector a (vstypeOf x)) @-}
replicateM :: (Monad m, Storable a) => VSEXPTYPE s a -> Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM t n f = phonyvtype t $ \p -> (\v -> proxyW v p) <$> G.replicateM n f

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
{-@ assume generateM :: x:VSEXPTYPE s a -> Int -> (Int -> m a) -> m (TVector a (vstypeOf x)) @-}
generateM :: (Monad m, Storable a) => VSEXPTYPE s a -> Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM t n f = phonyvtype t $ \p -> (\v -> proxyW v p) <$> G.generateM n f

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
{-@ assume create :: x:VSEXPTYPE s a -> (forall r. GHC.ST.ST r (TMVector r a (vstypeOf x))) -> TVector a (vstypeOf x) @-}
create :: Storable a => VSEXPTYPE s a -> (forall r. ST r (MVector r a)) -> Vector a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create t f = phonyvtype t $ \p -> unW $ G.create (Mutable.withW p <$> f)

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

{-@ assume force :: x:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
force :: Storable a => Vector a -> Vector a
{-# INLINE force #-}
force v = phony $ unW . proxyFW G.force v

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
{-@ assume (//) :: x:Vector a -> [(Int, a)] -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
(//) :: Storable a
     => Vector a   -- ^ initial vector (of length @m@)
     -> [(Int, a)]      -- ^ list of index/value pairs (of length @n@)
     -> Vector a
{-# INLINE (//) #-}
(//) v l = phony $ unW . proxyFW (G.// l) v

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector ty and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial Vector ty at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
update_ :: VECTOR s ty a
        => Vector ty a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector ty a   -- ^ value vector (of length @n2@)
        -> Vector ty a
{-# INLINE update_ #-}
update_ = G.update_
-}

-- | Same as ('//') but without bounds checking.
{-@ assume unsafeUpd :: x:Vector a -> [(Int, a)] -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
unsafeUpd :: Storable a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd v l = phony $ unW . proxyFW (`G.unsafeUpd` l) v

{-
-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: VECTOR s ty a => Vector ty a -> Vector Int -> Vector ty a -> Vector ty a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_
-}

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
{-@ assume accum :: (a -> b -> a) -> x:Vector a -> [(Int,b)] -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
accum :: Storable a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
{-# INLINE accum #-}
accum f v l = phony $ unW . proxyFW (\w -> G.accum f w l) v

{-
-- | /O(m+min(n1,n2))/ For each index @i@ from the index Vector ty and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial Vector ty at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
accumulate_ :: (VECTOR s ty a, VECTOR s ty b)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector ty a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector ty b      -- ^ value vector (of length @n2@)
            -> Vector ty a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_
-}

-- | Same as 'accum' but without bounds checking.
{-@ assume unsafeAccum :: (a -> b -> a) -> x:Vector a -> [(Int,b)] -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
unsafeAccum :: Storable a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum f v l = phony $ unW . proxyFW (\w -> G.unsafeAccum f w l) v

{-
-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (VECTOR s ty a, VECTOR s ty b) =>
               (a -> b -> a) -> Vector ty a -> Vector Int -> Vector ty b -> Vector ty a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_
-}

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
{-@ assume reverse :: x:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf x) @-}
reverse :: Storable a => Vector a -> Vector a
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
modify :: VECTOR s ty a => (forall s. MVector a -> ST s ()) -> Vector ty a -> Vector ty a
{-# INLINE modify #-}
modify p = G.modify p
-}

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
{-@ assume map :: (a -> b) -> x:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf x) @-}
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map f v = phony $ unW . proxyFW (G.map f) v

-- | /O(n)/ Apply a function to every element of a Vector ty and its index
{-@ assume imap :: (Int -> a -> b) -> x:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf x) @-}
imap :: (Storable a, Storable b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap f v = phony $ unW . proxyFW (G.imap f) v

-- | Map a function over a Vector ty and concatenate the results.
{-@ assume concatMap :: x:VSEXPTYPE s b -> (a -> TVector b (vstypeOf x)) -> Vector a -> TVector b (vstypeOf x) @-}
concatMap :: (Storable a, Storable b)
          => VSEXPTYPE s b
          -> (a -> Vector b)
          -> Vector a
          -> Vector b
{-# INLINE concatMap #-}
#if MIN_VERSION_vector(0,11,0)
concatMap t f v = phonyvtype t $ \p ->
    let v' = G.stream (withW p v)
    in proxyW (G.unstream $ Bundle.fromStream (Stream.concatMap (sElems . G.stream . withW p . f) (sElems v')) Unknown) p
#else
concatMap f v =
    phony $ \p ->
    (`proxyW` p) $
    G.unstream $
    Stream.concatMap (G.stream . withW p . f) $
    G.stream $
    withW p v
#endif

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
{-@ assume mapM :: (a -> m b) -> x:Vector a -> m (TVector b (Data.Vector.SEXP.vtypeOf x)) @-}
mapM :: (Monad m, Storable a, Storable b) => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM f v = phony $ \p -> unW <$> proxyFW (G.mapM f) v p

-- | /O(n)/ Apply the monadic action to all elements of a Vector ty and ignore the
-- results
{-@ mapM_ :: (a -> m b) -> Vector a -> m () @-}
mapM_ :: (Monad m, Storable a) => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ f v = phony $ proxyFW (G.mapM_ f) v

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
{-@ assume forM :: x:Vector a -> (a -> m b) -> m (TVector b (Data.Vector.SEXP.vtypeOf x)) @-}
forM :: (Monad m, Storable a, Storable b) => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM v f = phony $ \p -> unW <$> proxyFW (`G.forM` f) v p

-- | /O(n)/ Apply the monadic action to all elements of a Vector ty and ignore the
-- results. Equivalent to @flip 'mapM_'@.
{-@ assume forM_ :: Vector a -> (a -> m b) -> m () @-}
forM_ :: (Monad m, Storable a) => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ v f = phony $ proxyFW (`G.forM_` f) v

-- Zipping
-- -------
#if MIN_VERSION_vector(0,11,0)
smallest :: [Size] -> Size
smallest = List.foldl1' smaller
#endif

-- | /O(min(m,n))/ Zip two vectors with the given function.
{-@
assume zipWith
  :: x:VSEXPTYPE s c -> (a -> b -> c) -> Vector a -> Vector b -> TVector c (vstypeOf x)
@-}
zipWith :: (Storable a, Storable b, Storable c)
        => VSEXPTYPE s c -> (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
#if MIN_VERSION_vector(0,11,0)
zipWith t f xs ys = phonyvtype t $ \p ->
    let xs' = G.stream (withW p xs)
        ys' = G.stream (withW p ys)
        sz  = smaller (sSize xs') (sSize ys')
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith f (sElems xs') (sElems ys')) sz) p
#else
zipWith t f xs ys = phonyvtype t $ \p ->
   proxyW (G.unstream (Stream.zipWith f (G.stream (withW p xs)) (G.stream (withW p ys)))) p
#endif

-- | Zip three vectors with the given function.
{-@
assume zipWith3
  :: x:VSEXPTYPE s d -> (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> TVector d (vstypeOf x)
@-}
zipWith3 :: (Storable a, Storable b, Storable c, Storable d)
         => VSEXPTYPE s d -> (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith3 t f as bs cs = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        sz  = smallest [sSize as', sSize bs', sSize cs']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith3 f (sElems as') (sElems bs') (sElems cs')) sz) p
#else
zipWith3 t f as bs cs = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith3 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)))) p
#endif

{-@
assume zipWith4
  :: x:VSEXPTYPE s e
  -> (a -> b -> c -> d -> e)
  -> Vector a -> Vector b -> Vector c -> Vector d -> TVector e (vstypeOf x)
@-}
zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
         => VSEXPTYPE s e
         -> (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith4 t f as bs cs ds = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith4 f (sElems as') (sElems bs') (sElems cs') (sElems ds')) sz) p
#else
zipWith4 t f as bs cs ds = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith4 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)))) p
#endif

{-@
assume zipWith5 :: x:VSEXPTYPE s f
         -> (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> TVector f (vstypeOf x)
@-}
zipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f)
         => VSEXPTYPE s f
         -> (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith5 t f as bs cs ds es = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        es' = G.stream (withW p es)
        sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith5 f (sElems as') (sElems bs') (sElems cs') (sElems ds') (sElems es')) sz) p
#else
zipWith5 t f as bs cs ds es = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith5 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)))) p
#endif

{-@
assume zipWith6 :: x:VSEXPTYPE s g
         -> (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> TVector g (vstypeOf x)
@-}
zipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Storable g)
         => VSEXPTYPE s g
         -> (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith6 t f as bs cs ds es fs = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        es' = G.stream (withW p es)
        fs' = G.stream (withW p fs)
        sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es', sSize fs']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith6 f (sElems as') (sElems bs') (sElems cs') (sElems ds') (sElems es') (sElems fs')) sz) p
#else
zipWith6 t f as bs cs ds es fs = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith6 f (G.stream (withW p as)) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)) (G.stream (withW p fs)))) p
#endif

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
{-@
assume izipWith
  :: x:VSEXPTYPE s c -> (Int -> a -> b -> c) -> Vector a -> Vector b -> TVector c (vstypeOf x)
@-}
izipWith :: (Storable a, Storable b, Storable c)
         => VSEXPTYPE s c -> (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
#if MIN_VERSION_vector(0,11,0)
izipWith t f as bs = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        sz  = smaller (sSize as') (sSize bs')
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith (uncurry f) (Stream.indexed (sElems as')) (sElems bs')) sz) p
#else
izipWith t f as bs = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)))) p
#endif

-- | Zip three vectors and their indices with the given function.
{-@
assume izipWith3
  :: x:VSEXPTYPE s d -> (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> TVector d (vstypeOf x)
@-}
izipWith3 :: (Storable a, Storable b, Storable c, Storable d)
          => VSEXPTYPE s d
          -> (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith3 t f as bs cs = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        sz  = smallest [sSize as', sSize bs', sSize cs']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith3 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs')) sz) p
#else
izipWith3 t f as bs cs = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)))) p
#endif

{-@
assume izipWith4
  :: x:VSEXPTYPE s e
  -> (Int -> a -> b -> c -> d -> e)
  -> Vector a -> Vector b -> Vector c -> Vector d -> TVector e (vstypeOf x)
@-}
izipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
         => VSEXPTYPE s e
         -> (Int -> a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith4 t f as bs cs ds = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        sz  = smallest [ sSize as', sSize bs', sSize cs', sSize ds']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith4 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds')) sz) p
#else
izipWith4 t f as bs cs ds = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)))) p
#endif

{-@
assume izipWith5 :: x:VSEXPTYPE s f
         -> (Int -> a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> TVector f (vstypeOf x)
@-}
izipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f)
         => VSEXPTYPE s f
         -> (Int -> a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE izipWith5 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith5 t f as bs cs ds es = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        es' = G.stream (withW p es)
        sz  = smallest [ sSize as', sSize bs', sSize cs', sSize ds', sSize es']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith5 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds') (sElems es')) sz) p
#else
izipWith5 t f as bs cs ds es = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)))) p
#endif

{-@
assume izipWith6 :: x:VSEXPTYPE s g
         -> (Int -> a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> TVector g (vstypeOf x)
@-}
izipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Storable g)
         => VSEXPTYPE s g
         -> (Int -> a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith6 t f as bs cs ds es fs = phonyvtype t $ \p ->
    let as' = G.stream (withW p as)
        bs' = G.stream (withW p bs)
        cs' = G.stream (withW p cs)
        ds' = G.stream (withW p ds)
        es' = G.stream (withW p es)
        fs' = G.stream (withW p fs)
        sz  = smallest [ sSize as', sSize bs', sSize cs', sSize ds', sSize es', sSize fs']
    in proxyW (G.unstream $ Bundle.fromStream (Stream.zipWith6 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds') (sElems es') (sElems fs')) sz) p
#else
izipWith6 t f as bs cs ds es fs = phonyvtype t $ \p ->
  proxyW (G.unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (G.stream (withW p as))) (G.stream (withW p bs)) (G.stream (withW p cs)) (G.stream (withW p ds)) (G.stream (withW p es)) (G.stream (withW p fs)))) p
#endif

-- Monadic zipping
-- ---------------


-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
{-@
assume zipWithM :: (MonadR m, Storable a, Storable b, Storable c)
         => x:VSEXPTYPE s c
         -> (a -> b -> m c)
         -> Vector a
         -> Vector b
         -> m (TVector c (vstypeOf x))
@-}
zipWithM :: (MonadR m, Storable a, Storable b, Storable c)
         => VSEXPTYPE s c
         -> (a -> b -> m c)
         -> Vector a
         -> Vector b
         -> m (Vector c)
{-# INLINE zipWithM #-}
#if MIN_VERSION_vector(0,11,0)
zipWithM t f xs ys = phonyvtype t $ \p ->
    let xs' = lift $ G.stream (withW p xs)
        ys' = lift $ G.stream (withW p ys)
        sz  = smaller (sSize xs') (sSize ys')
    in proxyW <$> Prelude.fmap G.unstream (Bundle.unsafeFromList sz <$> Stream.toList (Stream.zipWithM f (sElems xs') (sElems ys')))
              <*> pure p
#else
zipWithM t f xs ys = phonyvtype t $ \p ->
    proxyW <$>
    unstreamM (Stream.zipWithM f (G.stream (withW p xs)) (G.stream (withW p ys))) <*>
    return p
  where
    -- Inlined from vector-0.10, which doesn't export unstreamM.
    unstreamM s = do
        zs <- MStream.toList s
        return $ G.unstream $ Stream.unsafeFromList (MStream.size s) zs
#endif

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, Storable a, Storable b)
          => (a -> b -> m c)
          -> Vector a
          -> Vector b
          -> m ()
{-# INLINE zipWithM_ #-}
#if MIN_VERSION_vector(0,11,0)
zipWithM_ f xs ys = phony $ \p ->
    let xs' = lift $ G.stream (withW p xs)
        ys' = lift $ G.stream (withW p ys)
    in Stream.zipWithM_ f (sElems xs') (sElems ys')
#else
zipWithM_ f xs ys = phony $ \p ->
    Stream.zipWithM_ f (G.stream (withW p xs)) (G.stream (withW p ys))
#endif

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
{-@ assume filter :: Storable a => (a -> Bool) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
filter :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter f v = phonyt (vtypeOf v) $ unW . proxyFW (G.filter f) v

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
{-@ assume ifilter :: Storable a => (Int -> a -> Bool) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
ifilter :: Storable a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter f v = phonyt (vtypeOf v) $ unW . proxyFW (G.ifilter f) v

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
{-@ assume filterM :: (a -> m Bool) -> v:Vector a -> m (TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
filterM :: (Monad m, Storable a) => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM f v = phonyt (vtypeOf v) $ \p -> unW <$> proxyFW (G.filterM f) v p

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- with copying.
{-@ assume takeWhile :: (a -> Bool) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
takeWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile f v = phony $ unW . proxyFW (G.takeWhile f) v

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- with copying.
{-@ assume dropWhile :: (a -> Bool) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
dropWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile f v = phony $ unW . proxyFW (G.dropWhile f) v

-- Partitioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
{-@ assume partition :: (a -> Bool) -> v:Vector a -> (TVector a (Data.Vector.SEXP.vtypeOf v), TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
partition :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition f v = phonyt (vtypeOf v) $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.partition f) v

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
{-@ assume unstablePartition :: (a -> Bool) -> v:Vector a -> (TVector a (Data.Vector.SEXP.vtypeOf v), TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
unstablePartition :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.unstablePartition f) v

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest with copying.
{-@ assume span :: (a -> Bool) -> v:Vector a -> (TVector a (Data.Vector.SEXP.vtypeOf v), TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
span :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.span f) v

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest with copying.
{-@ assume break :: (a -> Bool) -> v:Vector a -> (TVector a (Data.Vector.SEXP.vtypeOf v), TVector a (Data.Vector.SEXP.vtypeOf v)) @-}
break :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break f v = phony $ (\(a,b) -> (unW a, unW b)) . proxyFW (G.break f) v

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem a v = phony $ proxyFW (G.elem a) v

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem a v = phony $ proxyFW (G.notElem a) v

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Storable a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find f v = phony $ proxyFW (G.find f) v

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Storable a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f v = phony $ proxyFW (G.findIndex f) v

{-
-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: VECTOR s ty a => (a -> Bool) -> Vector ty a -> Vector Int
{-# INLINE findIndices #-}
findIndices f v = phony $ proxyFW (G.findIndices f) v
-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Storable a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex a v = phony $ proxyFW (G.elemIndex a) v

{-
-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (VECTOR s ty a, Eq a) => a -> Vector ty a -> Vector 'R.Int Int32
{-# INLINE elemIndices #-}
elemIndices s v = phony $ unW . proxyFW (G.elemIndices s) v
-}

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl f s v = phony $ proxyFW (G.foldl f s) v

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 f v = phony $ proxyFW (G.foldl1 f) v

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' f s v = phony $ proxyFW (G.foldl' f s) v

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' f v  = phony $ proxyFW (G.foldl1' f) v

-- | /O(n)/ Right fold
foldr :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr f s v = phony $ proxyFW (G.foldr f s) v

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 f v = phony $ proxyFW (G.foldr1 f) v

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' f s v = phony $ proxyFW (G.foldr' f s) v

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' f v = phony $ proxyFW (G.foldr1' f) v

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl f s v = phony $ proxyFW (G.ifoldl f s) v

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' f s  v = phony $ proxyFW (G.ifoldl' f s) v

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr f s v = phony $ proxyFW (G.ifoldr f s) v

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' f s v = phony $ proxyFW (G.ifoldr' f s) v

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all f v = phony $ \p -> G.all f (withW p v)

-- | /O(n)/ Check if any element satisfies the predicate.
any :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any f v = phony $ \p -> G.any f (withW p v)

-- -- | /O(n)/ Check if all elements are 'True'
-- and :: Vector 'Logical Bool -> Bool
-- {-# INLINE and #-}
-- and v = phony $ \p -> G.and (withW p v)
--
-- -- | /O(n)/ Check if any element is 'True'
-- or :: Vector 'Logical Bool -> Bool
-- {-# INLINE or #-}
-- or v = phony $ \p -> G.or (withW p v)

-- | /O(n)/ Compute the sum of the elements
sum :: (Storable a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum v = phony $ proxyFW G.sum v

-- | /O(n)/ Compute the produce of the elements
product :: (Storable a, Num a) => Vector a -> a
{-# INLINE product #-}
product v = phony $ proxyFW G.product v

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum v = phony $ proxyFW G.maximum v

-- | /O(n)/ Yield the maximum element of the Vector ty according to the given
-- comparison function. The vector may not be empty.
maximumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy f v = phony $ proxyFW (G.maximumBy f) v

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum v = phony $ proxyFW G.minimum v

-- | /O(n)/ Yield the minimum element of the Vector ty according to the given
-- comparison function. The vector may not be empty.
minimumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy f v = phony $ proxyFW (G.minimumBy f) v

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex v = phony $ proxyFW G.maxIndex v

-- | /O(n)/ Yield the index of the maximum element of the Vector ty according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy f v = phony $ proxyFW (G.maxIndexBy f) v

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex v = phony $ proxyFW G.minIndex v

-- | /O(n)/ Yield the index of the minimum element of the Vector ty according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy f v = phony $ proxyFW (G.minIndexBy f) v

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM f s v = phony $ proxyFW (G.foldM f s) v

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M f v = phony $ proxyFW (G.fold1M f) v

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' f s v = phony $ proxyFW (G.foldM' f s) v

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' f v = phony $ proxyFW (G.fold1M' f) v

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ f s v = phony $ proxyFW (G.foldM_ f s) v

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ f v = phony $ proxyFW (G.fold1M_ f) v

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ f s v = phony $ proxyFW (G.foldM'_ f s) v

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m ()
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
{-@ assume prescanl :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
prescanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.prescanl f s) v

-- | /O(n)/ Prescan with strict accumulator
{-@ assume prescanl' :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
prescanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.prescanl' f s) v

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
{-@ assume postscanl :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
postscanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.postscanl f s) v

-- | /O(n)/ Scan with strict accumulator
{-@ assume postscanl' :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
postscanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.postscanl' f s) v

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
{-@ assume scanl :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanl f s) v

-- | /O(n)/ Haskell-style scan with strict accumulator
{-@ assume scanl' :: (a -> b -> a) -> a -> v:Vector b -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanl' f s) v

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
{-@ assume scanl1 :: (a -> a -> a) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanl1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 f v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanl1 f) v

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
{-@ assume scanl1' :: (a -> a -> a) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanl1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' f v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanl1' f) v

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
{-@ assume prescanr :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
prescanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.prescanr f s) v

-- | /O(n)/ Right-to-left prescan with strict accumulator
{-@ assume prescanr' :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
prescanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.prescanr' f s) v

-- | /O(n)/ Right-to-left scan
{-@ assume postscanr :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
postscanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.postscanr f s) v

-- | /O(n)/ Right-to-left scan with strict accumulator
{-@ assume postscanr' :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
postscanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.postscanr' f s) v

-- | /O(n)/ Right-to-left Haskell-style scan
{-@ assume scanr :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
scanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanr f s) v

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
{-@ assume scanr' :: (a -> b -> b) -> b -> v:Vector a -> TVector b (Data.Vector.SEXP.vtypeOf v) @-}
scanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' f s v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanr' f s) v

-- | /O(n)/ Right-to-left scan over a non-empty vector
{-@ assume scanr1 :: (a -> a -> a) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanr1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 f v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanr1 f) v

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
{-@ assume scanr1' :: (a -> a -> a) -> v:Vector a -> TVector a (Data.Vector.SEXP.vtypeOf v) @-}
scanr1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' f v = phonyt (vtypeOf v) $ unW . proxyFW (G.scanr1' f) v

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: Storable a => Vector a -> [a]
{-# INLINE toList #-}
toList v = phony $ proxyFW G.toList v

-- | /O(n)/ Convert a list to a vector
{-@ assume fromList :: forall a . Storable a => t:VSEXPTYPE s a -> [a] -> TVector a (vstypeOf t) @-}
fromList :: Storable a => VSEXPTYPE s a -> [a] -> Vector a
{-# INLINE fromList #-}
fromList t xs = phonyvtype t $ proxyW (G.fromListN (Prelude.length xs) xs)

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Storable a => VSEXPTYPE s a -> Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN t i l = phonyvtype t $ proxyW (G.fromListN i l)

-- Conversions - Unsafe casts
-- --------------------------

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one with
-- copying. The mutable vector may not be used after this operation.
{-@ assume unsafeFreeze :: mv:Data.Vector.SEXP.Mutable.MVector (Region m) a -> m (TVector a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf mv)) @-}
unsafeFreeze :: (Storable a, MonadR m) => MVector (Region m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze m = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (Mutable.mvtypeOf m)) $ \p -> unW <$> G.unsafeFreeze (Mutable.withW p m)

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one with
-- copying. The immutable vector may not be used after this operation.
{-@ assume unsafeThaw :: v:Vector a -> m (TMVector (Region m) a (Data.Vector.SEXP.vtypeOf v)) @-}
unsafeThaw :: (MonadR m, Storable a) => Vector a -> m (MVector (Region m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw v = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (vtypeOf v)) $ \p -> Mutable.unW <$> G.unsafeThaw (withW p v)

-- | /O(n)/ Yield a mutable copy of the immutable vector.
{-@ assume thaw :: v:Vector a -> m (TMVector (Region m) a (Data.Vector.SEXP.vtypeOf v)) @-}
thaw :: (MonadR m, Storable a) => Vector a -> m (MVector (Region m) a)
{-# INLINE thaw #-}
thaw v1 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (vtypeOf v1)) $ \p -> Mutable.unW <$> G.thaw (withW p v1)

-- | /O(n)/ Yield an immutable copy of the mutable vector.
{-@ assume freeze :: mv:Data.Vector.SEXP.Mutable.MVector (Region m) a -> m (TVector a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf mv)) @-}
freeze :: (Storable a, MonadR m) => MVector (Region m) a -> m (Vector a)
{-# INLINE freeze #-}
freeze m1 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (Mutable.mvtypeOf m1)) $ \p -> unW <$> G.freeze (Mutable.withW p m1)

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
{-@ assume unsafeCopy :: mv:Data.Vector.SEXP.Mutable.MVector (Region m) a -> TVector a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf mv) -> m () @-}
unsafeCopy
  :: (MonadR m, Storable a)
  => MVector (Region m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy m1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (vtypeOf v2)) $ \p -> G.unsafeCopy (Mutable.withW p m1) (withW p v2)

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
{-@ assume copy :: mv:Data.Vector.SEXP.Mutable.MVector (Region m) a -> TVector a (Data.Vector.SEXP.Mutable.Internal.mvtypeOf mv) -> m () @-}
copy
  :: (MonadR m, Storable a)
  => MVector (Region m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy m1 v2 = do
    acquireIO <- getAcquireIO
    reify (acquireIO, evaluate (vtypeOf v2)) $ \p -> G.copy (Mutable.withW p m1) (withW p v2)


phony
  :: forall s r.
     (forall t. Reifies t (AcquireIO s, IO R.SEXPTYPE) => Proxy t -> r) -> r
phony f =
    reify acquireIO $ \p -> f p
  where
    acquireIO = violation "phony" "phony acquire or SEXPTYPE called."

phonyt
  :: R.SEXPTYPE ->
     (forall t. Reifies t (AcquireIO s, IO R.SEXPTYPE) => Proxy t -> r) -> r
phonyt t f =
    reify (acquireIO, evaluate t) $ \p -> f p
  where
    acquireIO = violation "phony" "phony acquire called."

phonyvtype
  :: VSEXPTYPE s a ->
     (forall t. Reifies t (AcquireIO s, IO R.SEXPTYPE) => Proxy t -> r) -> r
phonyvtype t f =
    reify (acquireIO, return (vstypeOf t)) $ \p -> f p
  where
    acquireIO = violation "phony" "phony acquire called."
