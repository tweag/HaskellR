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

-- | Pointer to SEXP structure
data SEXPREC = SEXPREC
{# pointer *SEXPREC as SEXP -> SEXPREC #}

--unSEXP :: SEXP -> Ptr SEXP
--unSEXP (SEXP x) = x

-- | Get the type of the object
{# fun TYPEOF as typeOf { id `SEXP' } -> `SEXPTYPE' toEnumG #}
-- | Read real type
{# fun REAL as real { id `SEXP' } -> `Ptr CDouble' id #} 
-- | read CAR object value
{# fun CAR as car { id `SEXP' } -> `SEXP' id #}
-- | read CDR object
{# fun CDR as cdr { id `SEXP' } -> `SEXP' id #}



-- | A vector element.
{#fun VECTOR_ELT as vectorElement { id `SEXP', `Int'} -> `SEXP' id #}

-- | Length of the vector
{# fun LENGTH as length { id `SEXP' } -> `Int' #}


-- | Access to the character info
{# fun R_CHAR as char { id `SEXP' } -> `String' #}
-- | Read a name from symbol
{# fun PRINTNAME as printName { id `SEXP' } -> `SEXP' id #}
-- | Read value from symbol
{# fun SYMVALUE as symValue { id `SEXP' } -> `SEXP' id #}
-- | Read internal value from symbol
{# fun INTERNAL as symInternal { id `SEXP' } -> `SEXP' id #}


-- | R boolean data type
{# pointer *Rboolean as Rboolean #}

-- | Interacive console swith, to set it one should use
-- @
-- poke rInteractive 1
-- @
foreign import ccall "&R_Interactive" rInteractive :: Ptr CInt

-- | Global nil value.
foreign import ccall "&R_NilValue"  nilValue  :: Ptr SEXP


-- | Create a String value inside R runtime.
{# fun Rf_mkString as mkString { `String' } -> `SEXP' id #}
{# fun Rf_PrintValue as printValue { id `SEXP'} -> `()' #}

-- | Protect variable from the garbage collector.
{# fun Rf_protect as protect { id `SEXP'} -> `SEXP' id #}
{# fun Rf_unprotect as unprotect { `Int' } -> `()' #}

--
toEnumG :: (Integral a, Enum b) => a -> b
toEnumG = toEnum . fromIntegral
