-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions. Nearly all structures
-- allocated internally in R are instances of a 'SEXPREC'. A pointer to
-- a 'SEXPREC' is called a 'SEXP'.
--
-- To allow for precise typing of bindings to primitive R functions, we index
-- 'SEXP's by 'SEXPTYPE', which classifies the /form/ of a 'SEXP' (see
-- "Foreign.R.Type"). A function accepting 'SEXP' arguments of any type should
-- leave the type index uninstantiated. A function returning a 'SEXP' result of
-- unknown type should use 'SomeSEXP'. (More precisely, unknown types in
-- /negative/ position should be /universally/ quantified and unknown types in
-- /positive/ position should be /existentially/ quantified).
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
#if __GLASGOW_HASKELL__ >= 710
-- We don't use ticks in this module, because they confuse c2hs.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif
module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXPTYPE(..)
  , R.Logical(..)
  , R.PairList
  , SEXP(..)
  , SomeSEXP(..)
  , unSomeSEXP
    -- * Casts and coercions
    -- $cast-coerce
  , cast
  , asTypeOf
  , unsafeCoerce
    -- * Node creation
  , allocSEXP
  , allocList
  , allocVector
  , allocVectorProtected
  , install
  , mkString
  , mkChar
  , CEType(..)
  , mkCharCE
  , mkWeakRef
    -- * Node attributes
  , typeOf
  , setAttribute
  , getAttribute
    -- * Node accessor functions
    -- ** Lists
  , cons
  , lcons
  , car
  , cdr
  , tag
  , setCar
  , setCdr
  , setTag
    -- ** Environments
  , envFrame
  , envEnclosing
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
  , unsafeSEXPToVectorPtr
  , unsafeVectorPtrToSEXP
  , indexVector
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
  , preserveObject
  , releaseObject
  , gc
    -- * Globals
  , isRInteractive
  , nilValue
  , unboundValue
  , missingArg
  , baseEnv
  , emptyEnv
  , globalEnv
  , signalHandlers
    -- * Communication with runtime
  , printValue
    -- * Low level info header access
  , SEXPInfo(..)
  , peekInfo
  , pokeInfo
  , mark
  , named
  -- * Internal types and functions
  --
  -- | Should not be used in user code. These exports are only needed for
  -- binding generation tools.
  , SEXPREC
  , SEXP0
  , sexp
  , unsexp
  , release
  , unsafeRelease
  , withProtected
  ) where

import Control.Memory.Region
import {-# SOURCE #-} Language.R.HExp (HExp)
import qualified Foreign.R.Type as R
import           Foreign.R.Type (SEXPTYPE, SSEXPTYPE)

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Exception (bracket)
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Bits
import Data.Complex
import Data.Int (Int32)
import Data.Singletons (fromSing)
#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
#endif
import Foreign (Ptr, castPtr, plusPtr, Storable(..))
#ifdef H_ARCH_WINDOWS
import Foreign (nullPtr)
#endif
import Foreign.C
import Prelude hiding (asTypeOf, length)

#define USE_RINTERNALS
#include <R.h>
#include <Rinterface.h>
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

data SEXPREC

-- | The basic type of all R expressions, classified by the form of the
-- expression, and the memory region in which it has been allocated.
newtype SEXP s (a :: SEXPTYPE) = SEXP { unSEXP :: Ptr (HExp s a) }
  deriving ( Eq
           , Ord
           , Storable
#if __GLASGOW_HASKELL__ < 710
           , Typeable
#endif
           )

instance Show (SEXP s a) where
  show (SEXP ptr) = show ptr

instance NFData (SEXP s a) where
  rnf = (`seq` ())

-- | 'SEXP' with no type index. This type and 'sexp' / 'unsexp'
-- are purely an artifact of c2hs (which doesn't support indexing a Ptr with an
-- arbitrary type in a @#pointer@ hook).
{#pointer SEXP as SEXP0 -> SEXPREC #}

-- | Add a type index to the pointer.
sexp :: SEXP0 -> SEXP s a
sexp = SEXP . castPtr

-- | Remove the type index from the pointer.
unsexp :: SEXP s a -> SEXP0
unsexp = castPtr . unSEXP

-- | Like 'sexp' but for 'SomeSEXP'.
somesexp :: SEXP0 -> SomeSEXP s
somesexp = SomeSEXP . sexp

-- | Release object into another region. Releasing is safe so long as the target
-- region is "smaller" than the source region, in the sense of
-- '(Control.Memory.Region.<=)'.
release :: (t <= s) => SEXP s a -> SEXP t a
release = unsafeRelease

unsafeRelease :: SEXP s a -> SEXP r a
unsafeRelease = sexp . unsexp

-- | A 'SEXP' of unknown form.
data SomeSEXP s = forall a. SomeSEXP {-# UNPACK #-} !(SEXP s a)

instance Show (SomeSEXP s) where
  show s = unSomeSEXP s show

instance Storable (SomeSEXP s) where
  sizeOf _ = sizeOf (undefined :: SEXP s a)
  alignment _ = alignment (undefined :: SEXP s a)
  peek ptr = SomeSEXP <$> peek (castPtr ptr)
  poke ptr (SomeSEXP s) = poke (castPtr ptr) s

instance NFData (SomeSEXP s) where
  rnf = (`seq` ())

-- | Deconstruct a 'SomeSEXP'. Takes a continuation since otherwise the
-- existentially quantified variable hidden inside 'SomeSEXP' would escape.
unSomeSEXP :: SomeSEXP s -> (forall a. SEXP s a -> r) -> r
unSomeSEXP (SomeSEXP s) k = k s

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cUIntToEnum :: Enum a => CUInt -> a
cUIntToEnum = toEnum . cIntConv

cUIntFromSingEnum :: SSEXPTYPE a -> CUInt
cUIntFromSingEnum = cIntConv . fromEnum . fromSing

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = cIntConv . fromEnum

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | Return the \"type\" tag (aka the form tag) of the given 'SEXP'. This
-- function is pure because the type of an object does not normally change over
-- the lifetime of the object.
typeOf :: SEXP s a -> SEXPTYPE
typeOf s = unsafeInlineIO $ cUIntToEnum <$> {#get SEXP->sxpinfo.type #} (unsexp s)

-- | read CAR object value
{#fun CAR as car { unsexp `SEXP s a' } -> `SomeSEXP s' somesexp #}

-- | read CDR object
{#fun CDR as cdr { unsexp `SEXP s a' } -> `SomeSEXP s' somesexp #}

-- | read object`s Tag
{# fun TAG as tag { unsexp `SEXP s a' } -> `SomeSEXP s' somesexp #}  --- XXX: add better constraint

-- | Set CAR field of object, when object is viewed as a cons cell.
setCar :: SEXP s a -> SEXP s b -> IO ()
setCar s s' = {#set SEXP->u.listsxp.carval #} (unsexp s) (castPtr $ unsexp s')

-- | Set CDR field of object, when object is viewed as a cons cell.
setCdr :: SEXP s a -> SEXP s b -> IO ()
setCdr s s' = {#set SEXP->u.listsxp.cdrval #} (unsexp s) (castPtr $ unsexp s')

-- | Set TAG field of object, when object is viewed as a cons cell.
setTag :: SEXP s a -> SEXP s b -> IO ()
setTag s s' = {#set SEXP->u.listsxp.tagval #} (unsexp s) (castPtr $ unsexp s')

--------------------------------------------------------------------------------
-- Coercion functions                                                         --
--------------------------------------------------------------------------------

-- $cast-coerce
--
-- /Coercions/ have no runtime cost, but are completely unsafe. Use with
-- caution, only when you know that a 'SEXP' is of the target type. /Casts/ are
-- safer, but introduce a runtime type check. The difference between the two is
-- akin to the difference between a C-style typecasts and C++-style
-- @dynamic_cast@'s.

unsafeCast :: SEXPTYPE -> SomeSEXP s -> SEXP s b
unsafeCast ty (SomeSEXP s)
  | ty == typeOf s = unsafeCoerce s
  | otherwise =
    error $ "cast: Dynamic type cast failed. Expected: " ++ show ty ++
            ". Actual: " ++ show (typeOf s) ++ "."

-- | Cast the type of a 'SEXP' into another type. This function is partial: at
-- runtime, an error is raised if the source form tag does not match the target
-- form tag.
cast :: SSEXPTYPE a -> SomeSEXP s -> SEXP s a
cast ty s = unsafeCast (fromSing ty) s

-- | Cast form of first argument to that of the second argument.
asTypeOf :: SomeSEXP s -> SEXP s a -> SEXP s a
asTypeOf s s' = typeOf s' `unsafeCast` s

-- | Unsafe coercion from one form to another. This is unsafe, in the sense that
-- using this function improperly could cause code to crash in unpredictable
-- ways. Contrary to 'cast', it has no runtime cost since it does not introduce
-- any dynamic check at runtime.
unsafeCoerce :: SEXP s a -> SEXP s b
unsafeCoerce = sexp . castPtr . unsexp

--------------------------------------------------------------------------------
-- Environment functions                                                      --
--------------------------------------------------------------------------------

-- | Environment frame.
{# fun FRAME as envFrame { unsexp `SEXP s R.Env' } -> `SEXP s R.PairList' sexp #}

-- | Enclosing environment.
{# fun ENCLOS as envEnclosing { unsexp `SEXP s R.Env' } -> `SEXP s R.Env' sexp #}

-- | Hash table associated with the environment, used for faster name lookups.
{# fun HASHTAB as envHashtab { unsexp `SEXP s R.Env' } -> `SEXP s R.Vector' sexp #}

--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

-- | Closure formals (aka the actual arguments).
{# fun FORMALS as closureFormals { unsexp `SEXP s R.Closure' } -> `SEXP s R.PairList' sexp #}

-- | The code of the closure.
{# fun BODY as closureBody { unsexp `SEXP s R.Closure' } -> `SomeSEXP s' somesexp #}

-- | The environment of the closure.
{# fun CLOENV as closureEnv { unsexp `SEXP s R.Closure' } -> `SEXP s R.Env' sexp #}

--------------------------------------------------------------------------------
-- Promise functions                                                          --
--------------------------------------------------------------------------------

-- | The code of a promise.
{# fun PRCODE as promiseCode { unsexp `SEXP s R.Promise'} -> `SomeSEXP s' somesexp #}

-- | The environment in which to evaluate the promise.
{# fun PRENV as promiseEnv { unsexp `SEXP s R.Promise'} -> `SEXP s R.Env' sexp #}

-- | The value of the promise, if it has already been forced.
{# fun PRVALUE as promiseValue { unsexp `SEXP s R.Promise'} -> `SomeSEXP s' somesexp #}

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
length :: R.IsVector a => SEXP s a -> IO Int
length s = fromIntegral <$> {#get VECSEXP->vecsxp.length #} (unsexp s)

-- | Read True Length vector field.
{#fun TRUELENGTH as trueLength `R.IsVector a' => { unsexp `SEXP s a' } -> `CInt' id #}

-- | Read character vector data
{#fun R_CHAR as char { unsexp `SEXP s R.Char' } -> `CString' id #}
-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

-- | Read real vector data.
{#fun REAL as real { unsexp `SEXP s R.Real' } -> `Ptr Double' castPtr #}

-- | Read integer vector data.
{#fun unsafe INTEGER as integer { unsexp `SEXP s R.Int' } -> `Ptr Int32' castPtr #}

-- | Read raw data.
{#fun RAW as raw { unsexp `SEXP s R.Raw' } -> `Ptr CChar' castPtr #}

-- XXX Workaround c2hs syntax limitations.
type Logical = 'R.Logical

-- | Read logical vector data.
{#fun LOGICAL as logical { unsexp `SEXP s Logical' } -> `Ptr R.Logical' castPtr #}

-- | Read complex vector data.
{#fun COMPLEX as complex { unsexp `SEXP s R.Complex' }
      -> `Ptr (Complex Double)' castPtr #}

-- | Read string vector data.
{#fun STRING_PTR as string { unsexp `SEXP s R.String'}
      -> `Ptr (SEXP s R.Char)' castPtr #}

-- | Extract the data pointer from a vector.
unsafeSEXPToVectorPtr :: SEXP s a -> Ptr ()
unsafeSEXPToVectorPtr s = (unsexp s) `plusPtr` {#sizeof SEXPREC_ALIGN #}

-- | Inverse of 'vectorPtr'.
unsafeVectorPtrToSEXP :: Ptr a -> SomeSEXP s
unsafeVectorPtrToSEXP s = SomeSEXP $ sexp $ s `plusPtr` (-{#sizeof SEXPREC_ALIGN #})

{# fun VECTOR_ELT as indexVector `R.IsGenericVector a'
     => { unsexp `SEXP s a', `Int' }
     -> `SomeSEXP s' somesexp #}

{# fun SET_VECTOR_ELT as writeVector `R.IsGenericVector a'
     => { unsexp `SEXP s a', `Int', unsexp `SEXP s b' }
     -> `SEXP s a' sexp #}

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
{#fun PRINTNAME as symbolPrintName { unsexp `SEXP s R.Symbol' } -> `SEXP s R.Char' sexp #}

-- | Read value from symbol.
{#fun SYMVALUE as symbolValue { unsexp `SEXP s R.Symbol' } -> `SEXP s a' sexp #}

-- | Read internal value from symbol.
{#fun INTERNAL as symbolInternal { unsexp `SEXP s R.Symbol' } -> `SEXP s a' sexp #}

--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Initialize a new string vector.
{#fun Rf_mkString as mkString { id `CString' } -> `SEXP V R.String' sexp #}

-- | Initialize a new character vector (aka a string).
{#fun Rf_mkChar as mkChar { id `CString' } -> `SEXP V R.Char' sexp #}

-- | Create Character value with specified encoding
{#fun Rf_mkCharCE as mkCharCE_ { id `CString', cIntFromEnum `CEType' } -> `SEXP V R.Char' sexp #}

mkCharCE :: CEType -> CString -> IO (SEXP V R.Char)
mkCharCE = flip mkCharCE_

-- | Intern a string @name@ into the symbol table.
--
-- If @name@ is not found, it is added to the symbol table. The symbol
-- corresponding to the string @name@ is returned.
{#fun Rf_install as install { id `CString' } -> `SEXP V R.Symbol' sexp #}

-- | Allocate a 'SEXP'.
{#fun Rf_allocSExp as allocSEXP { cUIntFromSingEnum `SSEXPTYPE a' }
      -> `SEXP V a' sexp #}

-- | Allocate a pairlist of 'SEXP's, chained together.
{#fun Rf_allocList as allocList { `Int' } -> `SEXP V R.List' sexp #}

-- | Allocate Vector.
{#fun Rf_allocVector as allocVector `R.IsVector a'
      => { cUIntFromSingEnum `SSEXPTYPE a',`Int' }
      -> `SEXP V a' sexp #}

allocVectorProtected :: (R.IsVector a) => SSEXPTYPE a -> Int -> IO (SEXP s a)
allocVectorProtected ty n = fmap release (protect =<< allocVector ty n)

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
{#fun Rf_cons as cons { unsexp `SEXP s a', unsexp `SEXP s b' } -> `SEXP V R.List' sexp #}

-- | Allocate a so-called cons cell of language objects, in essence a pair of
-- 'SEXP' pointers.
{#fun Rf_lcons as lcons { unsexp `SEXP s a', unsexp `SEXP s b' } -> `SEXP V R.Lang' sexp #}

-- | Print a string representation of a 'SEXP' on the console.
{#fun Rf_PrintValue as printValue { unsexp `SEXP s a'} -> `()' #}

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------

-- | Protect a 'SEXP' from being garbage collected by R. It is in particular
-- necessary to do so for objects that are not yet pointed by any other object,
-- e.g. when constructing a tree bottom-up rather than top-down.
--
-- To avoid unbalancing calls to 'protect' and 'unprotect', do not use these
-- functions directly but use 'Language.R.withProtected' instead.
{#fun Rf_protect as protect { unsexp `SEXP s a'} -> `SEXP G a' sexp #}

-- | @unprotect n@ unprotects the last @n@ objects that were protected.
{#fun Rf_unprotect as unprotect { `Int' } -> `()' #}

-- | Unprotect a specific object, referred to by pointer.
{#fun Rf_unprotect_ptr as unprotectPtr { unsexp `SEXP G a' } -> `()' #}

-- | Invoke an R garbage collector sweep.
{#fun R_gc as gc { } -> `()' #}

-- | Preserve an object accross GCs.
{#fun R_PreserveObject as preserveObject { unsexp `SEXP s a' } -> `()' #}

-- | Allow GC to remove an preserved object.
{#fun R_ReleaseObject as releaseObject { unsexp `SEXP s a' } -> `()' #}

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
{#fun Rf_eval as eval { unsexp `SEXP s a', unsexp `SEXP s R.Env' }
      -> `SomeSEXP V' somesexp #}

-- | Try to evaluate expression.
{#fun R_tryEval as tryEval { unsexp `SEXP s a', unsexp `SEXP s R.Env', id `Ptr CInt'}
      -> `SomeSEXP V' somesexp #}

-- | Try to evaluate without printing error/warning messages to stdout.
{#fun R_tryEvalSilent as tryEvalSilent
    { unsexp `SEXP s a', unsexp `SEXP s R.Env', id `Ptr CInt'}
      -> `SomeSEXP V' somesexp #}

-- | Construct a nullary function call.
{#fun Rf_lang1 as lang1 { unsexp `SEXP s a'} -> `SEXP V R.Lang' sexp #}

-- | Construct unary function call.
{#fun Rf_lang2 as lang2 { unsexp `SEXP s a', unsexp `SEXP s b'} -> `SEXP V R.Lang' sexp #}

-- | Construct a binary function call.
{#fun Rf_lang3 as lang3 { unsexp `SEXP s a', unsexp `SEXP s b', unsexp `SEXP s c'}
      -> `SEXP V R.Lang' sexp #}

-- | Find a function by name.
{#fun Rf_findFun as findFun { unsexp `SEXP s a', unsexp `SEXP s R.Env'}
      -> `SomeSEXP s' somesexp #}

-- | Find a variable by name.
{#fun Rf_findVar as findVar { unsexp `SEXP s a', unsexp `SEXP s R.Env'}
      -> `SEXP s R.Symbol' sexp #}

{#fun R_MakeWeakRef as mkWeakRef { unsexp `SEXP s a', unsexp `SEXP s b', unsexp `SEXP s c', cIntFromEnum `Bool' }
      -> `SEXP V R.WeakRef' sexp #}

--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

foreign import ccall "&R_Interactive" isRInteractive :: Ptr CInt

-- | Global nil value. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_NilValue" nilValue  :: Ptr (SEXP G R.Nil)

-- | Unbound marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_UnboundValue" unboundValue :: Ptr (SEXP G R.Symbol)

-- | Missing argument marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_MissingArg" missingArg :: Ptr (SEXP G R.Symbol)

-- | The base environment.
foreign import ccall "&R_BaseEnv" baseEnv :: Ptr (SEXP G R.Env)

-- | The empty environment.
foreign import ccall "&R_EmptyEnv" emptyEnv :: Ptr (SEXP G R.Env)

-- | Global environment.
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP G R.Env)

-- | Signal handler switch
foreign import ccall "&R_SignalHandlers" signalHandlers :: Ptr CInt

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

-- | Extract the header from the given 'SEXP'.
peekInfo :: SEXP s a -> IO SEXPInfo
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

-- | Write a new header.
pokeInfo :: SEXP s a -> SEXPInfo -> IO ()
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

-- | Set the GC mark.
mark :: Bool -> SEXP s a -> IO ()
mark b ts = {#set SEXP->sxpinfo.mark #} (unsexp ts) (if b then 1 else 0)

named :: Int -> SEXP s a -> IO ()
named v ts = {#set SEXP->sxpinfo.named #} (unsexp ts) (fromIntegral v)

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

-- | Get the attribute list from the given object.
getAttribute :: SEXP s a -> IO (SEXP s b)
getAttribute s = sexp . castPtr <$> ({#get SEXP->attrib #} (unsexp s))

-- | Set the attribute list.
setAttribute :: SEXP s a -> SEXP s b -> IO ()
setAttribute s v = {#set SEXP->attrib #} (unsexp s) (castPtr $ unsexp v)

-------------------------------------------------------------------------------
-- Encoding                                                                  --
-------------------------------------------------------------------------------

-- | Content encoding.
{#enum cetype_t as CEType {} deriving (Eq, Show) #}

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: IO (SEXP V a)      -- Action to acquire resource
              -> (SEXP s a -> IO b) -- Action
              -> IO b
withProtected create f =
    bracket
      (do { x <- create; _ <- protect x; return x })
      (const $ unprotect 1)
      (f . unsafeRelease)
