-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <R.h>
#include <Rinternals.h>
-- | TODO: group functions as in header
-- | TODO: add some way of typechecking
module Language.R.Foreign.Internal
  ( -- * Datatypes
    SEXPTYPE(..)
  , SEXP(..)
  , mkString
    -- * Cell attributes
  , typeOf
  , length
  , vectorELT
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

import Prelude hiding (length)
import Foreign
import Foreign.C

#c
typedef enum SEXPTYPE{
   NilSXP  = NILSXP,              /* 0 - nil = NULL */
   SymSXP  = SYMSXP,              /* 1 - symbols */
   ListSXP = LISTSXP,             /* 2 - lists of dotted pairs */
   //   CLOSXP  = 3,              /* closures */
   //   ENVSXP  = 4,              /* environments */
   //   PROMSXP = 5,              /* promises: [un]evaluated closure arguments */
   LangSXP = LANGSXP,             /* 6 - language constructs (special lists) */
   SpecialSXP = SPECIALSXP,       /* 7 - special forms */
   BuiltinSXP = BUILTINSXP,       /* 8 - builtin non-special forms */
   //   CHARSXP = 9,              /* "scalar" string type (internal only)*/
   //   LGLSXP  = 10,             /* logical vectors */
   IntSXP  = INTSXP,              /* 11 - integer vectors */
   RealSXP = REALSXP,             /* 14 - real variables */
   //   CPLXSXP = 15,             /* complex variables */
   //   STRSXP  = 16,             /* string vectors */
   //   DOTSXP  = 17,             /* dot-dot-dot object */
   //   ANYSXP  = 18,             /* make "any" args work */
   //   VECSXP  = 19,             /* generic vectors */
   ExpSXP = EXPRSXP,              /* 20 - expressions vectors */
   //   BCODESXP  = 21,           /* byte code */
   //   EXTPTRSXP = 22,           /* external pointer */
   //   WEAKREFSXP  = 23,         /* weak reference */
   //   RAWSXP  = 24,             /* raw bytes */
   //   S4SXP = 25,               /* S4 non-vector */
   //   NEWSXP      = 30,         /* fresh node creaed in new page */
   //   FREESXP     = 31,         /* node released by GC */
   FunSXP  = FUNSXP               /* 32 - Closure or Builtin */
};
#endc

-- | SEXP type representation.
{# enum SEXPTYPE {} deriving (Eq,Show) #}


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



-- Vector access attributes

-- | Length of the vector
{# fun LENGTH as length { id `SEXP' } -> `Int' #}

-- | Returns vector element
{# fun VECTOR_ELT as vectorELT { id `SEXP', `Int'} -> `SEXP' id #}

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
