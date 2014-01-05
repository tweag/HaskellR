-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE TypeFamilies #-}

module Data.Vector.SEXP
  ( ElemRep
  , Vector(..)
  , toSEXP
  , unsafeFromSEXP
    -- some reexports
  , length
  , head
  , toList
  , toString
  , toByteString
  ) where

import Data.Vector.SEXP.Base
import           Foreign.R ( SEXP )
import           Foreign.R.Type ( SEXPTYPE(Char), IsVector )

import qualified Data.Vector.Storable as Vector
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as B

import Control.Applicative
import Control.Arrow ( first )
import Data.Word ( Word8 )
import Foreign ( plusPtr, castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.ForeignPtr hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Prelude hiding ( length, head )

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

newtype Vector (ty :: SEXPTYPE) a = Vector { unVector :: Vector.Vector a }
    deriving (Eq)

-- | O(1) Create a vector from a 'SEXP'.
unsafeFromSEXP :: (E ty a, Storable a, IsVector ty)
               => SEXP ty
               -> IO (Vector ty a)
unsafeFromSEXP s = do
    len  <- {#get VECSEXP->vecsxp.length #} s
    fptr <- castForeignPtr <$>
              newForeignPtr_
                (s `plusPtr` {#sizeof SEXPREC_ALIGN #}) :: IO (ForeignPtr a)
    return $
      Vector $ Vector.unsafeFromForeignPtr0
                       fptr
                       (fromIntegral len)

toSEXP :: (E ty a, Storable a) => Vector ty a -> SEXP ty
toSEXP = (`plusPtr` (-{#sizeof SEXPREC_ALIGN #}))
       . castPtr
       . unsafeForeignPtrToPtr
       . fst
       . Vector.unsafeToForeignPtr0
       . unVector

length :: (E ty a, Storable a) => Vector ty a -> Int
length = Vector.length . unVector

head :: (E ty a, Storable a) => Vector ty a -> a
head = Vector.head . unVector

toList :: (E ty a, Storable a) => Vector ty a -> [a]
toList = Vector.toList . unVector

toString :: Vector 'Char Word8 -> String
toString v = unsafePerformIO $ peekCString . castPtr
         . unsafeForeignPtrToPtr . fst
         . Vector.unsafeToForeignPtr0
         . unVector $ v

toByteString :: Vector 'Char Word8 -> ByteString
toByteString = unsafePerformIO
             . B.unsafePackCStringLen
             . first castPtr
             . first unsafeForeignPtrToPtr
             . Vector.unsafeToForeignPtr0
             . unVector
