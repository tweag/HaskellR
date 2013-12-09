-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

module Data.Vector.SEXP
  ( Vector(..)
  , toSEXP
  , unsafeFromSEXP
    -- some reexports
  , length
  , head
  , toList
  , toString
  , toByteString
  ) where

import Prelude hiding ( length, head )
import qualified Foreign.R as R
import           Foreign.R ( SEXP )

import Control.Arrow ( first )
import Control.Applicative
import qualified Data.Vector.Storable as Vector
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as B
import Data.Word ( Word8 )
import Foreign ( plusPtr, castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.ForeignPtr hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import System.IO.Unsafe ( unsafePerformIO )

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

newtype Vector a = Vector { unVector :: Vector.Vector a }
    deriving (Eq, Show)

-- | Create Vectro from SEXP value.
unsafeFromSEXP :: Storable a => SEXP (R.Vector a) -> IO (Vector a)
unsafeFromSEXP s = do
    len  <- {#get VECSEXP->vecsxp.length #} s
    fptr <- castForeignPtr <$>
              newForeignPtr_
                (s `plusPtr` {#sizeof SEXPREC_ALIGN #}) :: IO (ForeignPtr a)
    return $
      Vector $ Vector.unsafeFromForeignPtr0
                       fptr
                       (fromIntegral len)

toSEXP :: Storable a => Vector a -> SEXP (R.Vector a)
toSEXP = (`plusPtr` (-{#sizeof SEXPREC_ALIGN #}))
       . castPtr
       . unsafeForeignPtrToPtr
       . fst
       . Vector.unsafeToForeignPtr0
       . unVector

length :: (Storable a) => Vector a -> Int
length = Vector.length . unVector

head :: (Storable a) => Vector a -> a
head = Vector.head . unVector

toList :: (Storable a) => Vector a -> [a]
toList = Vector.toList . unVector

toString :: Vector Word8 -> String
toString v = unsafePerformIO $ peekCString . castPtr
         . unsafeForeignPtrToPtr . fst
         . Vector.unsafeToForeignPtr0
         . unVector $ v

toByteString :: Vector Word8 -> ByteString
toByteString = unsafePerformIO
             . B.unsafePackCStringLen
             . first castPtr
             . first unsafeForeignPtrToPtr
             . Vector.unsafeToForeignPtr0
             . unVector
