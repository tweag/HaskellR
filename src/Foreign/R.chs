-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds  #-}

#include <R.h>
#include <Rinternals.h>
#include "missing_r.h"

module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXPTYPE(..)
  , SEXP(..)
  , SEXP0
  , readSEXPOffPtr
    -- * Node creation
  , allocSEXP
  , allocList
  , allocVector
  , install
  , mkString
    -- * Node attributes
  , typeOf
    -- * Node accessor functions
    -- ** Lists
  , car
  , cdr
  , tag
    -- ** Environments
  , envFrame
  , envClosure
  , envHashtab
    -- ** Closures
  , closureFormals
  , closureBody
  , closureEnv
    -- ** Promises
  , promiseCode
  , promiseEnv
  , promiseValue
    -- ** Symbols
  , symbolPrintName
  , symbolValue
  , symbolInternal
    -- ** Vectors
  , length
  , trueLength
  , index
  , char
  , real
  , integer
  , logical
  , complex
  , raw
  , string
  , vector
  , expression
    -- * Evaluation
  , tryEval
  , lang1
  , lang2
  , lang3
  , findFun
    -- * GC functions
  , protect
  , unprotect
    -- * Globals
  , globalEnv
  , nilValue
  , rInteractive
    -- * Communication with runtime
  , printValue
  ) where

import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE)

import Control.Applicative ((<$>))
import Data.Complex
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

cUIntFromEnum :: Enum a => a -> CUInt
cUIntFromEnum = cIntConv . fromEnum

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | Get the type of the object.
{#fun TYPEOF as typeOf { unSEXP `SEXP a' } -> `SEXPTYPE' cIntToEnum #}

-- | read CAR object value
{#fun CAR as car { unSEXP `SEXP a' } -> `SEXP b' SEXP #}

-- | read CDR object
{#fun CDR as cdr { unSEXP `SEXP a' } -> `SEXP b' SEXP #}

-- | read object`s Tag
{# fun TAG as tag { unSEXP `SEXP a' } -> `SEXP b' SEXP #}  --- XXX: add better constraint

--------------------------------------------------------------------------------
-- Environment functions                                                  --
--------------------------------------------------------------------------------

{# fun FRAME as envFrame {unSEXP `SEXP R.Env' } -> `SEXP a' SEXP #}
-- | read Environement frame
{# fun ENCLOS as envClosure {unSEXP `SEXP R.Env' } -> `SEXP a' SEXP #}
-- | read Environement frame
{# fun HASHTAB as envHashtab {unSEXP `SEXP R.Env' } -> `SEXP a' SEXP #}

--------------------------------------------------------------------------------
-- Closure functions                                                  --
--------------------------------------------------------------------------------

{# fun FORMALS as closureFormals {unSEXP `SEXP R.Closure' } -> `SEXP a' SEXP #}
{# fun BODY as closureBody {unSEXP `SEXP R.Closure' } -> `SEXP a' SEXP #}
{# fun CLOENV as closureEnv {unSEXP `SEXP R.Closure' } -> `SEXP R.Env' SEXP #}

--------------------------------------------------------------------------------
-- Promise functions                                                  --
--------------------------------------------------------------------------------
{# fun PRCODE as promiseCode {unSEXP `SEXP R.Promise'} -> `SEXP a' SEXP #}
{# fun PRENV as promiseEnv {unSEXP `SEXP R.Promise'} -> `SEXP a' SEXP #}
{# fun PRVALUE as promiseValue {unSEXP `SEXP R.Promise'} -> `SEXP a' SEXP #}


--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
{#fun LENGTH as length { unSEXP `SEXP (R.Vector a)' } -> `Int' #}

-- | A vector element.
{#fun VECTOR_ELT as index { unSEXP `SEXP (R.Vector (SEXP a))', `Int'} -> `SEXP a' SEXP #}

-- | Read True Length vector field
{#fun TRUELENGTH as trueLength { unSEXP `SEXP (R.Vector a)' } -> `CInt' id #}

-- | Read character vector data
{#fun R_CHAR as char { unSEXP `SEXP (R.Vector Word8)' } -> `CString' id #}
-- XXX: check if we really need Word8 here, maybe some better handling of endoding

-- | Read real vector data
{#fun REAL as real { unSEXP `SEXP (R.Vector CDouble)' } -> `Ptr CDouble' id #}

-- | Read integer vector data
{#fun INTEGER as integer { unSEXP `SEXP (R.Vector Int32)' } -> `Ptr CInt' id #}

-- | Read raw data
{#fun RAW as raw { unSEXP `SEXP (R.Vector Word8)' } -> `Ptr CChar' castPtr #}

-- | Read logical vector data
{#fun LOGICAL as logical { unSEXP `SEXP (R.Vector Bool)' } -> `Ptr CInt' id #}

-- | Read complex vector data
{#fun COMPLEX as complex { unSEXP `SEXP (R.Vector (Complex Double))' } -> `Ptr (Complex Double)' castPtr #}

-- | Read string vector data
{#fun STRING_PTR as string {unSEXP `SEXP (R.Vector (SEXP (R.Vector Word8)))'} -> `Ptr (SEXP (R.Vector Word8))' castPtr #}

-- | Read any SEXP vector data
{#fun INNER_VECTOR as vector {unSEXP `SEXP (R.Vector (SEXP R.Any))'} -> `Ptr (SEXP R.Any)' castPtr #}

-- | Read expression vector data
{#fun INNER_VECTOR as expression {unSEXP `SEXP (R.Vector (SEXP R.Expr))'} -> `Ptr (SEXP R.Expr)' castPtr #}

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
{#fun PRINTNAME as symbolPrintName { unSEXP `SEXP R.Symbol' } -> `SEXP (R.Vector Word8)' SEXP #}

-- | Read value from symbol.
{#fun SYMVALUE as symbolValue { unSEXP `SEXP R.Symbol' } -> `SEXP a' SEXP #}

-- | Read internal value from symbol.
{#fun INTERNAL as symbolInternal { unSEXP `SEXP R.Symbol' } -> `SEXP a' SEXP #}

--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Create a String value inside R runtime.
{#fun Rf_mkString as mkString { `String' } -> `SEXP (R.Vector Word8)' SEXP #}

-- | Create symbol by string name
{#fun Rf_install as install { `String' } -> `SEXP R.Symbol' SEXP #}

-- | Allocate SEXP
{#fun Rf_allocSExp as allocSEXP { cUIntFromEnum `SEXPTYPE' } -> `SEXP a' SEXP #}

-- | Allocate List
{#fun Rf_allocList as allocList { `Int' } -> `SEXP R.List' SEXP #}

-- | Allocate Vector
{#fun Rf_allocVector as allocVector { cUIntFromEnum `SEXPTYPE',`Int'} -> `SEXP (R.Vector a)' SEXP #}

{#fun Rf_PrintValue as printValue { unSEXP `SEXP a'} -> `()' #}

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------


-- | Protect variable from the garbage collector.
{#fun Rf_protect as protect { unSEXP `SEXP a'} -> `SEXP a' SEXP #}
{#fun Rf_unprotect as unprotect { `Int' } -> `()' #}

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | evaluate expression
{#fun R_tryEval as tryEval { unSEXP `SEXP a', unSEXP `SEXP R.Env', id `Ptr CInt'} -> `SEXP b' SEXP #}

-- | construct 1 arity expression
{#fun Rf_lang1 as lang1 { unSEXP `SEXP a'} -> `SEXP R.Lang' SEXP #}

-- | construct 1 arity expression
{#fun Rf_lang2 as lang2 { unSEXP `SEXP a', unSEXP `SEXP b'} -> `SEXP R.Lang' SEXP #}

-- | construct 1 arity expression
{#fun Rf_lang3 as lang3 { unSEXP `SEXP a', unSEXP `SEXP b', unSEXP `SEXP c'} -> `SEXP R.Lang' SEXP #}

-- |  find function by name
{#fun Rf_findFun as findFun { unSEXP `SEXP a', unSEXP `SEXP R.Env'} -> `SEXP c' SEXP #}
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

-- | Global environment
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP0)
