-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXPTYPE(..)
  , R.Logical(..)
  , SEXP
  , SEXPREC
  , SEXP0
  , sexp
  , unsexp
  , SomeSEXP(..)
  , unSomeSEXP
  , CEType(..)
    -- * Casts and coercions
    -- $cast-coerce
  , cast
  , asTypeOf
  , unsafeCoerce
    -- * Node creation
  , allocSEXP
  , allocList
  , allocVector
  , install
  , mkString
  , mkChar
  , mkCharCE
    -- * Node attributes
  , typeOf
  , setTypeOf
  , setAttribute
  , getAttribute
    -- * Node accessor functions
    -- ** Lists
  , cons
  , car
  , cdr
  , tag
  , setCar
  , setCdr
  , setTag
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
  , char
  , real
  , integer
  , logical
  , complex
  , raw
  , string
  , vector
  , writeVector
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
  , pokeInfo
  , mark
  , named
  ) where

import {-# SOURCE #-} Language.R.HExp
import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE)

import Control.Applicative
import Data.Bits
import Data.Complex
import Data.Int (Int32)
import Foreign (Ptr, castPtr, plusPtr, Storable(..))
#ifdef H_ARCH_WINDOWS
import Foreign (nullPtr)
#endif
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (asTypeOf, length)

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

data SomeSEXP = forall a. SomeSEXP {-# UNPACK #-} !(SEXP a)

instance Storable SomeSEXP where
  sizeOf _ = sizeOf (undefined :: SEXP a)
  alignment _ = alignment (undefined :: SEXP a)
  peek ptr = SomeSEXP <$> peek (castPtr ptr)
  poke ptr (SomeSEXP s) = poke (castPtr ptr) s

-- | Deconstruct a 'SomeSEXP'. Takes a continuation since otherwise the
-- existentially quantified variable hidden inside 'SomeSEXP' would escape.
unSomeSEXP :: SomeSEXP -> (forall a. SEXP a -> r) -> r
unSomeSEXP (SomeSEXP s) k = k s

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cUIntToEnum :: Enum a => CUInt -> a
cUIntToEnum = toEnum . cIntConv

cUIntFromEnum :: Enum a => a -> CUInt
cUIntFromEnum = cIntConv . fromEnum

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = cIntConv . fromEnum

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

typeOf :: SEXP a -> SEXPTYPE
typeOf s = unsafePerformIO $ cUIntToEnum <$> {#get SEXP->sxpinfo.type #} s

setTypeOf :: SEXPTYPE -> SEXP a -> IO (SEXP b)
setTypeOf t s = ({#set SEXP->sxpinfo.type #} s (cUIntFromEnum t)) >> return (unsafeCoerce s)

-- | read CAR object value
{#fun CAR as car { unsexp `SEXP a' } -> `SEXP b' sexp #}

-- | read CDR object
{#fun CDR as cdr { unsexp `SEXP a' } -> `SEXP b' sexp #}

-- | read object`s Tag
{# fun TAG as tag { unsexp `SEXP a' } -> `SEXP b' sexp #}  --- XXX: add better constraint

-- | Set CAR field of object.
setCar :: SEXP a -> SEXP b -> IO ()
setCar s s' = {#set SEXP->u.listsxp.carval #} (castPtr s) (castPtr s')

-- | Set CDR field of object.
setCdr :: SEXP a -> SEXP b -> IO ()
setCdr s s' = {#set SEXP->u.listsxp.cdrval #} (castPtr s) (castPtr s')

-- | Set TAG field of object.
setTag :: SEXP a -> SEXP b -> IO ()
setTag s s' = {#set SEXP->u.listsxp.tagval #} (castPtr s) (castPtr s')

--------------------------------------------------------------------------------
-- Coercion functions                                                         --
--------------------------------------------------------------------------------

-- $cast-coerce
--
-- Coercions have no runtime cost, but are completely unsafe. Use with caution,
-- only when you know that a 'SEXP' is of the target type.

-- | Cast the type of a 'SEXP' into another type. This function is partial: at
-- runtime, an error is raised if the source form tag does not match the target
-- form tag.
cast :: SEXPTYPE -> SEXP a -> SEXP b
cast ty s
  | ty == typeOf s = unsafeCoerce s
  | otherwise = error "cast: Dynamic type cast failed."

-- | Cast form of first argument to that of the second argument.
asTypeOf :: SomeSEXP -> SEXP a -> SEXP a
asTypeOf (SomeSEXP s) s' = typeOf s' `cast` s

-- | Unsafe coercion from one form to another. This is unsafe, in the sense that
-- using this function improperly could cause code to crash in unpredictable
-- ways. Contrary to 'cast', it has no runtime cost since it does not introduce
-- any dynamic check at runtime.
unsafeCoerce :: SEXP a -> SEXP b
unsafeCoerce = castPtr

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
{#fun LENGTH as length `R.IsVector a' => { unsexp `SEXP a' } -> `Int' #}

-- | Read True Length vector field.
{#fun TRUELENGTH as trueLength `R.IsVector a' => { unsexp `SEXP a' } -> `CInt' id #}

-- | Read character vector data
{#fun R_CHAR as char { unsexp `SEXP R.Char' } -> `CString' id #}
-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

-- | Read real vector data.
{#fun REAL as real { unsexp `SEXP R.Real' } -> `Ptr Double' castPtr #}

-- | Read integer vector data.
{#fun INTEGER as integer { unsexp `SEXP R.Int' } -> `Ptr Int32' castPtr #}

-- | Read raw data.
{#fun RAW as raw { unsexp `SEXP R.Raw' } -> `Ptr CChar' castPtr #}

-- XXX Workaround c2hs syntax limitations.
type Logical = 'R.Logical

-- | Read logical vector data.
{#fun LOGICAL as logical { unsexp `SEXP Logical' } -> `Ptr R.Logical' castPtr #}

-- | Read complex vector data.
{#fun COMPLEX as complex { unsexp `SEXP R.Complex' }
      -> `Ptr (Complex Double)' castPtr #}

-- | Read string vector data.
{#fun STRING_PTR as string { unsexp `SEXP R.String'}
      -> `Ptr (SEXP R.Char)' castPtr #}

-- | Read any SEXP vector data.
vector :: SEXP a -> IO (Ptr b)
vector s = return $ s `plusPtr` {#sizeof SEXPREC_ALIGN #}

{# fun SET_VECTOR_ELT as writeVector `R.IsGenericVector a' => { unsexp `SEXP a', `Int', unsexp `SEXP b'}
       -> `SEXP b' sexp #}

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
{#fun PRINTNAME as symbolPrintName { unsexp `SEXP R.Symbol' } -> `SEXP R.Char' sexp #}

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
{#fun Rf_mkString as mkString { id `CString' } -> `SEXP R.String' sexp #}

{#fun Rf_mkChar as mkChar { id `CString' } -> `SEXP R.Char' sexp #}

-- | Create Character value with specified encoding
{#fun Rf_mkCharCE as mkCharCE { id `CString', cIntFromEnum `CEType' } -> `SEXP R.Char' sexp #}

-- | Probe the symbol table
--
-- If "name" is not found, it is installed in the symbol table.
-- The symbol corresponding to the string "name" is returned.
{#fun Rf_install as install { id `CString' } -> `SEXP R.Symbol' sexp #}

-- | Allocate SEXP.
{#fun Rf_allocSExp as allocSEXP { cUIntFromEnum `SEXPTYPE' } -> `SEXP a' sexp #}

-- | Allocate List.
{#fun Rf_allocList as allocList { `Int' } -> `SEXP R.List' sexp #}

-- | Allocate Vector.
{#fun Rf_allocVector as allocVector `R.IsVector a' => { cUIntFromEnum `SEXPTYPE',`Int'} -> `SEXP a' sexp #}

{#fun Rf_cons as cons { unsexp `SEXP a', unsexp `SEXP b' } -> `SEXP R.List' sexp #}

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

pokeInfo :: SEXP a -> SEXPInfo -> IO ()
pokeInfo (unsexp -> s) i = do
    {#set SEXP->sxpinfo.type  #} s (fromIntegral.fromEnum $ infoType i)
    {#set SEXP->sxpinfo.obj   #} s (if infoObj  i then 1 else 0)
    {#set SEXP->sxpinfo.named #} s (fromIntegral $ infoNamed i)
    {#set SEXP->sxpinfo.gp    #} s (fromIntegral $ infoGp i)
    {#set SEXP->sxpinfo.mark  #} s (if infoMark i  then 1 else 0)
    {#set SEXP->sxpinfo.debug #} s (if infoDebug i then 1 else 0)
    {#set SEXP->sxpinfo.trace #} s (if infoTrace i then 1 else 0)
    {#set SEXP->sxpinfo.spare #} s (if infoSpare i then 1 else 0)
    {#set SEXP->sxpinfo.gcgen #} s (fromIntegral $ infoGcGen i)
    {#set SEXP->sxpinfo.gccls #} s (fromIntegral $ infoGcCls i)

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

-------------------------------------------------------------------------------
-- Encoding                                                                  --
-------------------------------------------------------------------------------

{#enum cetype_t as CEType {} deriving (Eq, Show) #}
