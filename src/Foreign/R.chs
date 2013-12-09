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
  , SEXPREC
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
  , mkChar
    -- * Node attributes
  , typeOf
  , setAttribute
  , getAttribute
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
  , indexExpr
  , char
  , real
  , integer
  , logical
  , complex
  , raw
  , string
  , vector
  , expression
  , setExprElem
    -- * Evaluation
  , eval
  , tryEval
  , tryEvalSilent
  , lang1
  , lang2
  , lang3
  , findFun
  , findVar
    -- * GC functions
  , protect
  , unprotect
  , unprotectPtr
  , gc
    -- * Globals
  , globalEnv
  , baseEnv
  , nilValue
  , unboundValue
  , missingArg
  , rInteractive
  , rInputHandlers
    -- * Communication with runtime
  , printValue
  , processEvents
#ifdef H_ARCH_UNIX
  , processGUIEventsUnix
#endif
    -- * Low level info header access
  , SEXPInfo(..)
  , peekInfo
  , mark
  , named
  ) where

import {-# SOURCE #-} H.HExp
import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE)

import Control.Applicative
import Data.Complex
import Foreign
import Foreign.C
import Prelude hiding (length)

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <R_ext/Memory.h>
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
-- Environment functions                                                      --
--------------------------------------------------------------------------------

{# fun FRAME as envFrame { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}
-- | read Environement frame
{# fun ENCLOS as envClosure { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}
-- | read Environement frame
{# fun HASHTAB as envHashtab { unsexp `SEXP R.Env' } -> `SEXP a' sexp #}

--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

{# fun FORMALS as closureFormals { unsexp `SEXP R.Closure' } -> `SEXP a' sexp #}
{# fun BODY as closureBody { unsexp `SEXP R.Closure' } -> `SEXP a' sexp #}
{# fun CLOENV as closureEnv { unsexp `SEXP R.Closure' } -> `SEXP R.Env' sexp #}

--------------------------------------------------------------------------------
-- Promise functions                                                          --
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

-- | Expression element.
{# fun VECTOR_ELT as indexExpr { unsexp `SEXP R.Expr', `Int'} -> `SEXP a' sexp #}

-- | Set expression element.
{# fun SET_VECTOR_ELT as setExprElem { unsexp `SEXP R.Expr', `Int', unsexp `SEXP a'} -> `SEXP a' sexp #}

-- | Read True Length vector field.
{#fun TRUELENGTH as trueLength { unsexp `SEXP (R.Vector a)' } -> `CInt' id #}

-- | Read character vector data
{#fun R_CHAR as char { unsexp `SEXP (R.Vector Word8)' } -> `CString' id #}
-- XXX: check if we really need Word8 here, maybe some better handling of 
-- encoding

-- | Read real vector data.
{#fun REAL as real { unsexp `SEXP (R.Vector Double)' } -> `Ptr Double' castPtr #}

-- | Read integer vector data.
{#fun INTEGER as integer { unsexp `SEXP (R.Vector Int32)' } -> `Ptr Int32' castPtr #}

-- | Read raw data.
{#fun RAW as raw { unsexp `SEXP (R.Vector Word8)' } -> `Ptr CChar' castPtr #}

-- | Read logical vector data.
{#fun LOGICAL as logical { unsexp `SEXP (R.Vector Bool)' } -> `Ptr Bool' castPtr #}

-- | Read complex vector data.
{#fun COMPLEX as complex { unsexp `SEXP (R.Vector (Complex Double))' }
      -> `Ptr (Complex Double)' castPtr #}

-- | Read string vector data.
{#fun STRING_PTR as string { unsexp `SEXP (R.Vector (SEXP (R.Vector Word8)))'} 
      -> `Ptr (SEXP (R.Vector Word8))' castPtr #}

-- | Read any SEXP vector data.
{#fun INNER_VECTOR as vector { unsexp `SEXP (R.Vector (SEXP R.Any))'}
      -> `Ptr (SEXP R.Any)' castPtr #}

-- | Read expression vector data
{#fun INNER_VECTOR as expression { unsexp `SEXP (R.Vector (SEXP R.Expr))'}
      -> `Ptr (SEXP R.Expr)' castPtr #}

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
{#fun Rf_mkString as mkString { id `CString' } -> `SEXP (R.String)' sexp #}


{#fun Rf_mkChar as mkChar { id `CString' } -> `SEXP (R.Vector Word8)' sexp #}

-- | Probe the symbol table
--
-- If "name" is not found, it is installed in the symbol table.
-- The symbol corresponding to the string "name" is returned.
{#fun Rf_install as install { id `CString' } -> `SEXP R.Symbol' sexp #}

-- | Allocate SEXP.
{#fun Rf_allocSExp as allocSEXP { cUIntFromEnum `SEXPTYPE' } -> `SEXP a' sexp #}

-- | Allocate List.
{#fun Rf_allocList as allocList { `Int' } -> `SEXP R.List' sexp #}

-- | Allocate Vecto.r
{#fun Rf_allocVector as allocVector { cUIntFromEnum `SEXPTYPE',`Int'} -> `SEXP (R.Vector a)' sexp #}

{#fun Rf_PrintValue as printValue { unsexp `SEXP a'} -> `()' #}

{#fun R_ProcessEvents as processEvents {} -> `()' #}

#ifdef H_ARCH_UNIX
{#fun processGUIEventsUnix { id `Ptr (Ptr ())' } -> `()' #}
#endif

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------


-- | Protect variable from the garbage collector.
{#fun Rf_protect as protect { unsexp `SEXP a'} -> `SEXP a' sexp #}
{#fun Rf_unprotect as unprotect { `Int' } -> `()' #}
{#fun Rf_unprotect_ptr as unprotectPtr { unsexp `SEXP a' } -> `()' #}
{#fun R_gc as gc { } -> `()' #}

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate expression.
{#fun Rf_eval as eval { unsexp `SEXP a', unsexp `SEXP R.Env' } -> `SEXP b' sexp #}

-- | Try to evaluate expression.
{#fun R_tryEval as tryEval { unsexp `SEXP a', unsexp `SEXP R.Env', id `Ptr CInt'} -> `SEXP b' sexp #}

-- | Try to evaluate without printing error/warning messages to stdout.
{#fun R_tryEvalSilent as  tryEvalSilent { unsexp `SEXP a', unsexp `SEXP R.Env', id `Ptr CInt'} -> `SEXP b' sexp #}

-- | Construct 1 arity expression.
{#fun Rf_lang1 as lang1 { unsexp `SEXP a'} -> `SEXP R.Lang' sexp #}

-- | Construct 2 arity expression.
{#fun Rf_lang2 as lang2 { unsexp `SEXP a', unsexp `SEXP b'} -> `SEXP R.Lang' sexp #}

-- | Construct 3 arity expression.
{#fun Rf_lang3 as lang3 { unsexp `SEXP a', unsexp `SEXP b', unsexp `SEXP c'} -> `SEXP R.Lang' sexp #}

-- | Find function by name.
{#fun Rf_findFun as findFun { unsexp `SEXP a', unsexp `SEXP R.Env'} -> `SEXP c' sexp #}

-- | Find variable by name.
{#fun Rf_findVar as findVar { unsexp `SEXP a', unsexp `SEXP R.Env'} -> `SEXP R.Symbol' sexp #}
--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

-- | Interacive console swith, to set it one should use.
-- @
-- poke rInteractive 1
-- @
foreign import ccall "&R_Interactive" rInteractive :: Ptr CInt

-- | Global nil value.
foreign import ccall "&R_NilValue" nilValue  :: Ptr (SEXP R.Nil)

-- | Global environment.
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP R.Env)

-- | Unbound marker.
foreign import ccall "&R_UnboundValue" unboundValue :: Ptr (SEXP R.Symbol)

-- | The base environment; formerly nilValue.
foreign import ccall "&R_BaseEnv" baseEnv :: Ptr (SEXP R.Env)

-- | Missing argument marker.
foreign import ccall "&R_MissingArg" missingArg :: Ptr (SEXP R.Symbol)

-- | Input handlers used in event loops.
#ifdef H_ARCH_UNIX
foreign import ccall "&R_InputHandlers" rInputHandlers :: Ptr (Ptr ())
#else
rInputHandlers :: Ptr (Ptr ())
rInputHandlers = nullPtr
#endif

----------------------------------------------------------------------------------
-- Structure header                                                             --
----------------------------------------------------------------------------------

-- | Info header for the SEXP data structure.
data SEXPInfo = SEXPInfo
      { infoType  :: SEXPTYPE    -- ^ Type of the SEXP.
      , infoObj   :: Bool        -- ^ Is this an object with a class attribute.
      , infoNamed :: Int         -- ^ Control copying information.
      , infoGp    :: Int         -- ^ General purpose data.
      , infoMark  :: Bool        -- ^ Mark object as 'in use' in GC.
      , infoDebug :: Bool        -- ^ Debug marker.
      , infoTrace :: Bool        -- ^ Trace marker.
      , infoSpare :: Bool        -- ^ Alignment (not in use).
      , infoGcGen :: Int         -- ^ GC Generation.
      , infoGcCls :: Int         -- ^ GC Class of node.
      } deriving ( Show )

-- | Read header information for given SEXP structure.
peekInfo :: SEXP a -> IO SEXPInfo
peekInfo ts =
    SEXPInfo
      <$> (toEnum.fromIntegral <$> {#get SEXP->sxpinfo.type #} s)
      <*> ((/=0)               <$> {#get SEXP->sxpinfo.obj #} s)
      <*> (fromIntegral        <$> {#get SEXP->sxpinfo.named #} s)
      <*> (fromIntegral        <$> {#get SEXP->sxpinfo.gp #} s)
      <*> ((/=0)               <$> {#get SEXP->sxpinfo.mark #} s)
      <*> ((/=0)               <$> {#get SEXP->sxpinfo.debug #} s)
      <*> ((/=0)               <$> {#get SEXP->sxpinfo.trace #} s)
      <*> ((/=0)               <$> {#get SEXP->sxpinfo.spare #} s)
      <*> (fromIntegral        <$> {#get SEXP->sxpinfo.gcgen #} s)
      <*> (fromIntegral        <$> {#get SEXP->sxpinfo.gccls #} s)
  where
    s = unsexp ts

-- | Set GC mark.
mark :: Bool -> SEXP a -> IO ()
mark b ts = {#set SEXP->sxpinfo.mark #} (unsexp ts) (if b then 1 else 0)

named :: Int -> SEXP a -> IO ()
named v ts = {#set SEXP->sxpinfo.named #} (unsexp ts) (fromIntegral v)

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

getAttribute :: SEXP a -> IO (SEXP b)
getAttribute s = castPtr <$> ({#get SEXP->attrib #} (unsexp s))

setAttribute :: SEXP a -> SEXP b -> IO ()
setAttribute s v = {#set SEXP->attrib #} (unsexp s) (castPtr v)
