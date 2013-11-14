-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXPTYPE(..)
  , SEXP
  , SEXP0
  , sexp
  , unsexp
  , SomeSEXP(..)
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

import {-# SOURCE #-} H.HExp
import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE)

import Control.Applicative ((<$>))
import Data.Complex
import Foreign
import Foreign.C
import Prelude hiding (length)

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include "missing_r.h"

-- XXX temp workaround due to R bug: doesn't export R_CHAR when USE_RINTERNALS
-- is defined.
#c
const char *(R_CHAR)(SEXP x);
#endc

--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

-- | The basic type of all R expressions, classified by the form of the
-- expression.
type SEXP (a :: SEXPTYPE) = Ptr (HExp a)
data SEXPREC

-- | 'SEXP' with no type index. This type and 'sexp' / 'unsexp' are purely an
-- artifact of c2hs (which doesn't support indexing a Ptr with an arbitrary type
-- in a #pointer hook).
{#pointer SEXP as SEXP0 -> SEXPREC #}

sexp :: SEXP0 -> SEXP a
sexp = castPtr

unsexp :: SEXP a -> SEXP0
unsexp = castPtr

data SomeSEXP = forall a. SomeSEXP (SEXP a)

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cUIntToEnum :: Enum a => CUInt -> a
cUIntToEnum = toEnum . cIntConv

cUIntFromEnum :: Enum a => a -> CUInt
cUIntFromEnum = cIntConv . fromEnum

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

typeOf :: SEXP a -> IO SEXPTYPE
typeOf s = cUIntToEnum <$> {#get SEXP->sxpinfo.type #} s

-- | read CAR object value
{#fun CAR as car { unsexp `SEXP a' } -> `SEXP b' sexp #}

-- | read CDR object
{#fun CDR as cdr { unsexp `SEXP a' } -> `SEXP b' sexp #}

-- | read object`s Tag
{# fun TAG as tag { unsexp `SEXP a' } -> `SEXP b' sexp #}  --- XXX: add better constraint

--------------------------------------------------------------------------------
-- Environment functions                                                  --
--------------------------------------------------------------------------------

{# fun FRAME as envFrame { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}
-- | read Environement frame
{# fun ENCLOS as envClosure { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}
-- | read Environement frame
{# fun HASHTAB as envHashtab { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}

--------------------------------------------------------------------------------
-- Closure functions                                                  --
--------------------------------------------------------------------------------

{# fun FORMALS as closureFormals { unsexp `SEXP R.Closure' } -> `SEXP a' sexp #}
{# fun BODY as closureBody { unsexp `SEXP R.Closure' } -> `SEXP a' sexp #}
{# fun CLOENV as closureEnv { unsexp `SEXP R.Closure' } -> `SEXP R.Env' sexp #}

--------------------------------------------------------------------------------
-- Promise functions                                                  --
--------------------------------------------------------------------------------
{# fun PRCODE as promiseCode { unsexp `SEXP R.Promise'} -> `SEXP a' sexp #}
{# fun PRENV as promiseEnv { unsexp `SEXP R.Promise'} -> `SEXP a' sexp #}
{# fun PRVALUE as promiseValue { unsexp `SEXP R.Promise'} -> `SEXP a' sexp #}


--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
{#fun LENGTH as length { unsexp `SEXP (R.Vector a)' } -> `Int' #}

-- | A vector element.
{#fun VECTOR_ELT as index { unsexp `SEXP (R.Vector (SEXP a))', `Int'} -> `SEXP a' sexp #}

-- | Read True Length vector field
{#fun TRUELENGTH as trueLength { unsexp `SEXP (R.Vector a)' } -> `CInt' id #}

-- | Read character vector data
{#fun R_CHAR as char { unsexp `SEXP (R.Vector Word8)' } -> `CString' id #}
-- XXX: check if we really need Word8 here, maybe some better handling of endoding

-- | Read real vector data
{#fun REAL as real { unsexp `SEXP (R.Vector Double)' } -> `Ptr Double' castPtr #}

-- | Read integer vector data
{#fun INTEGER as integer { unsexp `SEXP (R.Vector Int32)' } -> `Ptr CInt' id #}

-- | Read raw data
{#fun RAW as raw { unsexp `SEXP (R.Vector Word8)' } -> `Ptr CChar' castPtr #}

-- | Read logical vector data
{#fun LOGICAL as logical { unsexp `SEXP (R.Vector Bool)' } -> `Ptr CInt' id #}

-- | Read complex vector data
{#fun COMPLEX as complex { unsexp `SEXP (R.Vector (Complex Double))' } -> `Ptr (Complex Double)' castPtr #}

-- | Read string vector data
{#fun STRING_PTR as string { unsexp `SEXP (R.Vector (SEXP (R.Vector Word8)))'} -> `Ptr (SEXP (R.Vector Word8))' castPtr #}

-- | Read any SEXP vector data
{#fun INNER_VECTOR as vector { unsexp `SEXP (R.Vector (SEXP R.Any))'} -> `Ptr (SEXP R.Any)' castPtr #}

-- | Read expression vector data
{#fun INNER_VECTOR as expression { unsexp `SEXP (R.Vector (SEXP R.Expr))'} -> `Ptr (SEXP R.Expr)' castPtr #}

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
{#fun PRINTNAME as symbolPrintName { unsexp `SEXP R.Symbol' } -> `SEXP (R.Vector Word8)' sexp #}

-- | Read value from symbol.
{#fun SYMVALUE as symbolValue { unsexp `SEXP R.Symbol' } -> `SEXP a' sexp #}

-- | Read internal value from symbol.
{#fun INTERNAL as symbolInternal { unsexp `SEXP R.Symbol' } -> `SEXP a' sexp #}

--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Create a String value inside R runtime.
{#fun Rf_mkString as mkString { `String' } -> `SEXP (R.Vector Word8)' sexp #}

-- | Create symbol by string name
{#fun Rf_install as install { `String' } -> `SEXP R.Symbol' sexp #}

-- | Allocate SEXP
{#fun Rf_allocSExp as allocSEXP { cUIntFromEnum `SEXPTYPE' } -> `SEXP a' sexp #}

-- | Allocate List
{#fun Rf_allocList as allocList { `Int' } -> `SEXP R.List' sexp #}

-- | Allocate Vector
{#fun Rf_allocVector as allocVector { cUIntFromEnum `SEXPTYPE',`Int'} -> `SEXP (R.Vector a)' sexp #}

{#fun Rf_PrintValue as printValue { unsexp `SEXP a'} -> `()' #}

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------


-- | Protect variable from the garbage collector.
{#fun Rf_protect as protect { unsexp `SEXP a'} -> `SEXP a' sexp #}
{#fun Rf_unprotect as unprotect { `Int' } -> `()' #}

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | evaluate expression
{#fun R_tryEval as tryEval { unsexp `SEXP a', unsexp `SEXP R.Env', id `Ptr CInt'} -> `SEXP b' sexp #}

-- | construct 1 arity expression
{#fun Rf_lang1 as lang1 { unsexp `SEXP a'} -> `SEXP R.Lang' sexp #}

-- | construct 1 arity expression
{#fun Rf_lang2 as lang2 { unsexp `SEXP a', unsexp `SEXP b'} -> `SEXP R.Lang' sexp #}

-- | construct 1 arity expression
{#fun Rf_lang3 as lang3 { unsexp `SEXP a', unsexp `SEXP b', unsexp `SEXP c'} -> `SEXP R.Lang' sexp #}

-- |  find function by name
{#fun Rf_findFun as findFun { unsexp `SEXP a', unsexp `SEXP R.Env'} -> `SEXP c' sexp #}
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
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP R.Env)
