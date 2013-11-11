-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <R.h>
#include <Rinternals.h>
module Foreign.R
  ( module Foreign.R.Type
    -- * Datatypes
  , SEXPTYPE(..)
  , SEXP(..)
  , SEXP0
  , readSEXPOffPtr
  , mkString
    -- * Cell attributes
  , typeOf
  , length
  , vectorElement
  , char
  , real
    -- ** Objects accessors
  , car
  , cdr
    -- ** Symbol accessors
  , printName
  , symValue
  , symInternal
    -- * GC functions
  , protect
  , unprotect
    -- * Communication with runtime
  , rInteractive
  , nilValue
  , printValue
  ) where

import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE)

import Control.Applicative ((<$>))
import Foreign
import Foreign.C
import GHC.Storable (readPtrOffPtr)
import Prelude hiding (length)

--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

-- | The basic type of all R expressions, classified by the form of the
-- expression.
newtype SEXP (a :: SEXPTYPE) = SEXP { unSEXP :: Ptr SEXPREC }
data SEXPREC

-- | 'SEXP' with no type index. 'unSEXP' / 'SEXP' act as (un)marshallers.
{#pointer SEXP as SEXP0 -> SEXPREC #}

readSEXPOffPtr :: Ptr (SEXP a) -> Int -> IO (SEXP a)
readSEXPOffPtr ptr i = SEXP <$> readPtrOffPtr (castPtr ptr) i

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . cIntConv

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | Get the type of the object.
{#fun TYPEOF as typeOf { unSEXP `SEXP a' } -> `SEXPTYPE' cIntToEnum #}

-- | Read value of real type.
{#fun REAL as real { unSEXP `SEXP a' } -> `Ptr Prelude.Double' castPtr #}

-- | read CAR object value
{#fun CAR as car { unSEXP `SEXP a' } -> `SEXP a' SEXP #}

-- | read CDR object
{#fun CDR as cdr { unSEXP `SEXP a' } -> `SEXP a' SEXP #}

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
{#fun LENGTH as length { unSEXP `SEXP (R.Vector a)' } -> `Int' #}

-- | A vector element.
{#fun VECTOR_ELT as vectorElement { unSEXP `SEXP (R.Vector a)', `Int'} -> `SEXP a' SEXP #}

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
{#fun PRINTNAME as printName { unSEXP `SEXP a' } -> `SEXP a' SEXP #}

-- | Read value from symbol.
{#fun SYMVALUE as symValue { unSEXP `SEXP a' } -> `SEXP a' SEXP #}

-- | Read internal value from symbol.
{#fun INTERNAL as symInternal { unSEXP `SEXP a' } -> `SEXP a' SEXP #}

--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

-- | Conversion from R strings and C strings.
{#fun R_CHAR as char { unSEXP `SEXP R.Char' } -> `String' #}

-- | Create a String value inside R runtime.
{#fun Rf_mkString as mkString { `String' } -> `SEXP (R.Vector Word8)' SEXP #}
{#fun Rf_PrintValue as printValue { unSEXP `SEXP a'} -> `()' #}

-- | Protect variable from the garbage collector.
{#fun Rf_protect as protect { unSEXP `SEXP a'} -> `SEXP a' SEXP #}
{#fun Rf_unprotect as unprotect { `Int' } -> `()' #}

--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

-- | Interacive console swith, to set it one should use
-- @
-- poke rInteractive 1
-- @
foreign import ccall "&R_Interactive" rInteractive :: Ptr CInt

-- | Global nil value.
foreign import ccall "&R_NilValue" nilValue  :: Ptr (SEXP R.Nil)
