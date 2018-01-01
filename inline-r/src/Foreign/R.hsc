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
{-# LANGUAGE CApiFFI #-}
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

-- Warns about some sanity checks like IsVector, that has no methods and are
-- not used.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
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
  , isS4
  , setAttributes
  , getAttribute
  , getAttributes
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
  , readVector
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
  , interruptsPending
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
  -- * Deprecated
  , indexVector
  ) where

import Control.Memory.Region
import Foreign.R.Internal
import Foreign.R.Type
import Foreign.R.Type as R

import Control.Applicative
import Control.Exception (bracket)
import Data.Complex
import Data.Int (Int32)
#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
#endif
import Foreign (Ptr, castPtr) 
import Foreign.C
import Prelude hiding (asTypeOf, length)

#define USE_RINTERNALS
#include <Rinternals.h>

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | read CAR object value
car :: SEXP s a -> IO (SomeSEXP s)
car (unsexp -> s) = somesexp <$> c_car s

foreign import capi "Rinternals.h CAR" c_car :: SEXP0 -> IO SEXP0

-- | read CDR object
cdr :: SEXP s a -> IO (SomeSEXP s)
cdr (unsexp -> s) = somesexp <$> c_cdr s

foreign import capi "Rinternals.h CDR" c_cdr :: SEXP0 -> IO SEXP0

-- | read object`s Tag
tag :: SEXP s a -> IO (SomeSEXP s)
tag (unsexp -> s) = somesexp <$> c_tag s

--- XXX: add better constraint
foreign import capi "Rinternals.h TAG" c_tag :: SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Environment functions                                                      --
--------------------------------------------------------------------------------

envFrame :: (SEXP s 'R.Env) -> IO (SEXP s R.PairList)
envFrame (unsexp -> s) = sexp <$> c_frame s

foreign import capi "Rinternals.h FRAME" c_frame :: SEXP0 -> IO SEXP0

-- | Enclosing environment.
envEnclosing :: SEXP s 'R.Env -> IO (SEXP s 'R.Env)
envEnclosing (unsexp -> s) = sexp <$> c_enclos s

foreign import capi "Rinternals.h ENCLOS" c_enclos :: SEXP0 -> IO SEXP0

-- | Hash table associated with the environment, used for faster name lookups.
envHashtab :: SEXP s 'R.Env -> IO (SEXP s 'R.Vector)
envHashtab (unsexp -> s) = sexp <$> c_hashtab s

foreign import capi "Rinternals.h HASHTAB" c_hashtab :: SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

-- | Closure formals (aka the actual arguments).
closureFormals :: SEXP s 'R.Closure -> IO (SEXP s R.PairList)
closureFormals (unsexp -> s) = sexp <$> c_closureFormals s

-- | The code of the closure.
closureBody :: SEXP s 'R.Closure -> IO (SomeSEXP s)
closureBody (unsexp -> s) = somesexp <$> c_closureBody s

-- | The environment of the closure.
closureEnv :: SEXP s 'R.Closure -> IO (SEXP s 'R.Env)
closureEnv (unsexp -> s) = sexp <$> c_closureEnv s

foreign import capi "Rinternals.h FORMALS" c_closureFormals :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h BODY"    c_closureBody    :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h CLOENV"  c_closureEnv     :: SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Promise functions                                                          --
--------------------------------------------------------------------------------

-- | The code of a promise.
promiseCode :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseCode (unsexp -> s) = somesexp <$> c_promiseCode s

-- | The environment in which to evaluate the promise.
promiseEnv :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseEnv (unsexp -> s) = somesexp <$> c_promiseEnv s

-- | The value of the promise, if it has already been forced.
promiseValue :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseValue (unsexp -> s) = somesexp <$> c_promiseValue s

foreign import capi "Rinternals.h PRVALUE" c_promiseValue :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h PRENV"   c_promiseEnv   :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h PRCODE"  c_promiseCode  :: SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read True Length vector field.
trueLength :: R.IsVector a => SEXP s a -> IO CInt
trueLength (unsexp -> s) = c_trueLength s

foreign import capi "Rinternals.h TRUELENGTH" c_trueLength :: SEXP0 -> IO CInt

-- | Wrapper for CApi usage
data {-# CTYPE "const char" #-} ConstCString

-- | Read character vector data
char :: SEXP s 'R.Char -> IO CString
char (unsexp -> s) = castPtr <$> c_char s

foreign import capi "Rinternals.h CHAR" c_char :: SEXP0 -> IO (Ptr ConstCString)

-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

-- | Read real vector data.
real :: SEXP s 'R.Real -> IO (Ptr Double)
real (unsexp -> s) = c_real s

foreign import capi "Rinternals.h REAL" c_real :: SEXP0 -> IO (Ptr Double)

-- | Read integer vector data.
integer :: SEXP s 'R.Int -> IO (Ptr Int32)
integer (unsexp -> s) = c_integer s

foreign import capi "Rinternals.h INTEGER" c_integer :: SEXP0 -> IO (Ptr Int32)

-- | Read raw data.
raw :: SEXP s 'R.Raw -> IO (Ptr CChar)
raw (unsexp -> s) = c_raw s

foreign import capi "Rinternals.h RAW" c_raw :: SEXP0 -> IO (Ptr CChar)

-- | Read logical vector data.
logical :: SEXP s 'R.Logical -> IO (Ptr R.Logical)
logical (unsexp -> s) = c_logical s

foreign import capi "Rinternals.h LOGICAL" c_logical :: SEXP0 -> IO (Ptr R.Logical)

-- | Read complex vector data.
complex :: SEXP s 'R.Complex -> IO (Ptr (Complex Double))
complex (unsexp -> s) = c_complex s

foreign import capi "Rinternals.h COMPLEX" c_complex :: SEXP0 -> IO (Ptr (Complex Double))

-- | Read string vector data.
string :: SEXP s 'R.String -> IO (Ptr (SEXP s 'R.Char))
string (unsexp -> s) = castPtr <$> c_string s

foreign import capi "Rinternals.h STRING_PTR" c_string :: SEXP0 -> IO (Ptr SEXP0)

readVector :: R.IsGenericVector a => SEXP s a -> Int -> IO (SomeSEXP s)
readVector (unsexp -> s) n = somesexp <$> c_readVector s n

foreign import capi "Rinternals.h VECTOR_ELT" c_readVector :: SEXP0 -> Int -> IO SEXP0

indexVector :: IsGenericVector a => SEXP s a -> Int -> IO (SomeSEXP s)
{-# DEPRECATED indexVector "Use readVector instead." #-}
indexVector = readVector

writeVector :: R.IsGenericVector a => SEXP s a -> Int -> SEXP s b -> IO (SEXP s a)
writeVector (unsexp -> a) n (unsexp -> b) = sexp <$> c_writeVector a n b

foreign import capi "Rinternals.h SET_VECTOR_ELT" c_writeVector :: SEXP0 -> Int -> SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
symbolPrintName :: SEXP s 'R.Symbol -> IO (SEXP s a)
symbolPrintName (unsexp -> s) = sexp <$> c_symbolPrintName s

-- | Read value from symbol.
symbolValue :: SEXP s 'R.Symbol -> IO (SEXP s a)
symbolValue (unsexp -> s) = sexp <$> c_symbolValue s

-- | Read internal value from symbol.
symbolInternal :: SEXP s 'R.Symbol -> IO (SEXP s a)
symbolInternal (unsexp -> s) = sexp <$> c_symbolInternal s

foreign import capi "Rinternals.h PRINTNAME" c_symbolPrintName :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h SYMVALUE"  c_symbolValue     :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h INTERNAL"  c_symbolInternal  :: SEXP0 -> IO SEXP0

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Initialize a new string vector.
mkString :: CString -> IO (SEXP V 'R.String)
mkString value = sexp <$> c_mkString value

-- | Initialize a new character vector (aka a string).
mkChar :: CString -> IO (SEXP V 'R.Char)
mkChar val = sexp <$> c_mkChar val

-- | Create Character value with specified encoding
mkCharCE :: CEType -> CString -> IO (SEXP V 'R.Char)
mkCharCE (cIntFromEnum -> ce) value = sexp <$> c_mkCharCE value ce

foreign import capi "Rinternals.h Rf_mkString" c_mkString :: CString -> IO SEXP0
foreign import capi "Rinternals.h Rf_mkChar"   c_mkChar   :: CString -> IO SEXP0
foreign import capi "Rinternals.h Rf_mkCharCE" c_mkCharCE :: CString -> CInt -> IO SEXP0

-- | Intern a string @name@ into the symbol table.
--
-- If @name@ is not found, it is added to the symbol table. The symbol
-- corresponding to the string @name@ is returned.
install :: CString -> IO (SEXP V 'R.Symbol)
install name = sexp <$> c_install name
foreign import capi "Rinternals.h Rf_install" c_install :: CString -> IO SEXP0

-- | Allocate a 'SEXP'.
allocSEXP :: SSEXPTYPE a -> IO (SEXP V a)
allocSEXP (cUIntFromSingEnum -> s) = sexp <$> c_allocSEXP s

foreign import capi "Rinternals.h Rf_allocSExp" c_allocSEXP
  :: CUInt -> IO SEXP0

-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: Int -> IO (SEXP V 'R.List)
allocList n = sexp <$> c_allocList n
foreign import capi "Rinternals.h Rf_allocList" c_allocList :: Int -> IO SEXP0

-- | Allocate Vector.
allocVector :: R.IsVector a => SSEXPTYPE a -> Int -> IO (SEXP V a)
allocVector (cUIntFromSingEnum -> p) n = sexp <$> c_allocVector p n
foreign import capi "Rinternals.h Rf_allocVector" c_allocVector :: CUInt -> Int -> IO SEXP0

allocVectorProtected :: (R.IsVector a) => SSEXPTYPE a -> Int -> IO (SEXP s a)
allocVectorProtected ty n = fmap release (protect =<< allocVector ty n)

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: SEXP s a -> SEXP s b -> IO (SEXP V 'R.List)
cons (unsexp -> a) (unsexp -> b) = sexp <$> c_cons a b

foreign import capi "Rinternals.h Rf_cons" c_cons :: SEXP0 -> SEXP0 -> IO SEXP0

-- | Allocate a so-called cons cell of language objects, in essence a pair of
-- 'SEXP' pointers.
lcons :: SEXP s a -> SEXP s b -> IO (SEXP V 'R.Lang)
lcons (unsexp -> a) (unsexp -> b) = sexp <$> c_lcons a b

foreign import capi "Rinternals.h Rf_lcons" c_lcons :: SEXP0 -> SEXP0 -> IO SEXP0


printValue :: SEXP s a -> IO ()
printValue (unsexp -> s) = c_printValue s

foreign import capi "Rinternals.h Rf_PrintValue" c_printValue :: SEXP0 -> IO ()

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------

-- | Protect a 'SEXP' from being garbage collected by R. It is in particular
-- necessary to do so for objects that are not yet pointed by any other object,
-- e.g. when constructing a tree bottom-up rather than top-down.
--
-- To avoid unbalancing calls to 'protect' and 'unprotect', do not use these
-- functions directly but use 'Language.R.withProtected' instead.
protect :: SEXP s a -> IO (SEXP G a)
protect (unsexp -> s) = sexp <$> c_protect s

foreign import capi "Rinternals.h Rf_protect" c_protect :: SEXP0 -> IO SEXP0

-- | @unprotect n@ unprotects the last @n@ objects that were protected.
unprotect :: Int -> IO ()
unprotect i  = c_unprotect i

foreign import capi "Rinternals.h Rf_unprotect" c_unprotect :: Int -> IO ()

-- | Unprotect a specific object, referred to by pointer.
unprotectPtr :: SEXP G a -> IO ()
unprotectPtr (unsexp -> s) = c_unprotectPtr s

foreign import capi "Rinternals.h Rf_unprotect_ptr" c_unprotectPtr :: SEXP0 -> IO ()

-- | Invoke an R garbage collector sweep.
foreign import capi "Rinternals.h R_gc" gc :: IO ()

-- | Preserve an object accross GCs.
preserveObject :: SEXP s a -> IO ()
preserveObject (unsexp -> s) = c_preserveObject s

foreign import capi "Rinternals.h R_PreserveObject" c_preserveObject :: SEXP0 -> IO ()

-- | Allow GC to remove an preserved object.
releaseObject :: SEXP s a -> IO ()
releaseObject (unsexp -> s) = c_releaseObject s

foreign import capi "Rinternals.h R_ReleaseObject" c_releaseObject :: SEXP0 -> IO ()

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
eval :: SEXP s a -> SEXP s 'R.Env -> IO (SomeSEXP V)
eval (unsexp -> expr) (unsexp -> env) =
  somesexp <$> c_eval expr env

-- | Try to evaluate expression.
tryEval :: SEXP s a -> SEXP s 'R.Env -> Ptr CInt -> IO (SomeSEXP V)
tryEval (unsexp -> expr) (unsexp -> env) retCode =
  somesexp <$> c_tryEval expr env retCode

-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: SEXP  s a -> SEXP s 'R.Env -> Ptr CInt -> IO (SomeSEXP V)
tryEvalSilent (unsexp -> expr) (unsexp -> env) retCode =
  somesexp <$> c_tryEvalSilent expr env retCode

foreign import capi "Rinternals.h Rf_eval" c_eval :: SEXP0 -> SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h R_tryEval" c_tryEval :: SEXP0 -> SEXP0 -> Ptr CInt -> IO SEXP0
foreign import capi "Rinternals.h R_tryEvalSilent" c_tryEvalSilent :: SEXP0 -> SEXP0 -> Ptr CInt -> IO SEXP0

-- | Construct a nullary function call.
lang1 :: SEXP s a -> IO (SEXP V 'R.Lang)
lang1 (unsexp -> s) = sexp <$> c_lang1 s

-- | Construct unary function call.
lang2 :: SEXP s a -> SEXP s b ->  IO (SEXP V 'R.Lang)
lang2 (unsexp -> f) (unsexp -> x) = sexp <$> c_lang2 f x

-- | Construct a binary function call.
lang3 :: SEXP s a -> SEXP s b ->  SEXP s c -> IO (SEXP V 'R.Lang)
lang3 (unsexp -> f) (unsexp -> x) (unsexp -> y) = sexp <$> c_lang3 f x y

foreign import capi "Rinternals.h Rf_lang1" c_lang1 :: SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h Rf_lang2" c_lang2 :: SEXP0 -> SEXP0 -> IO SEXP0
foreign import capi "Rinternals.h Rf_lang3" c_lang3 :: SEXP0 -> SEXP0 -> SEXP0 -> IO SEXP0


-- | Find a function by name.
findFun :: SEXP s a -> SEXP s 'R.Env -> IO (SomeSEXP s)
findFun (unsexp -> a) (unsexp -> env) = somesexp <$> c_findFun a env

foreign import capi "Rinternals.h Rf_findFun" c_findFun :: SEXP0 -> SEXP0 -> IO SEXP0

-- | Find a variable by name.
findVar :: SEXP s a -> SEXP s 'R.Env -> IO (SEXP s 'R.Symbol)
findVar (unsexp -> a) (unsexp -> env) = sexp <$> c_findVar a env

foreign import capi "Rinternals.h Rf_findVar" c_findVar :: SEXP0 -> SEXP0 -> IO SEXP0

mkWeakRef :: SEXP s a -> SEXP s b -> SEXP s c -> Bool -> IO (SEXP V 'R.WeakRef)
mkWeakRef (unsexp -> a) (unsexp -> b) (unsexp -> c) (cIntFromEnum -> t) =
  sexp <$> c_mkWeakRef a b c t

foreign import capi "Rinternals.h R_MakeWeakRef" c_mkWeakRef :: SEXP0 -> SEXP0 -> SEXP0 -> CInt -> IO SEXP0

-------------------------------------------------------------------------------
-- Encoding                                                                  --
-------------------------------------------------------------------------------


-- | Content encoding.
data CEType
  = CE_Native
  | CE_UTF8
  | CE_Latin1
  | CE_Bytes
  | CE_Symbol
  | CE_Any
  deriving (Eq, Show)

instance Enum CEType where
  fromEnum CE_Native = #const CE_NATIVE
  fromEnum CE_UTF8   = #const CE_UTF8
  fromEnum CE_Latin1 = #const CE_LATIN1
  fromEnum CE_Bytes  = #const CE_BYTES
  fromEnum CE_Symbol = #const CE_SYMBOL
  fromEnum CE_Any    = #const CE_ANY
  toEnum i = case i of
    (#const CE_NATIVE) -> CE_Native
    (#const CE_UTF8)   -> CE_UTF8
    (#const CE_LATIN1) -> CE_Latin1
    (#const CE_BYTES)  -> CE_Bytes
    (#const CE_SYMBOL) -> CE_Symbol
    (#const CE_ANY)    -> CE_Any
    _ -> error "CEType.fromEnum: unknown tag"

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
