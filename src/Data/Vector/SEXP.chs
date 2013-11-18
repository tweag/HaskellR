-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# Language RankNTypes #-}
module Data.Vector.SEXP 
  ( Vector(..)
  , toSEXP
  , unsafeFromSEXP 
  ) where

import qualified Foreign.R as R
import           Foreign.R ( SEXP )

import Control.Applicative
import qualified Data.Vector.Storable as Vector
import Foreign ( plusPtr, castPtr )
import Foreign.C
import Foreign.Storable
import Foreign.ForeignPtr hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

newtype Vector a = Vector { unVector :: Vector.Vector a }

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

toSEXP :: forall a . Storable a => Vector a -> SEXP (R.Vector a)
toSEXP (Vector v) = castPtr (unsafeForeignPtrToPtr p)
  where 
    (p,_) = Vector.unsafeToForeignPtr0 v
