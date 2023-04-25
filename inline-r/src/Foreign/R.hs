-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions. Nearly all structures
-- allocated internally in R are instances of a 'SEXPREC'. A pointer to
-- a 'SEXPREC' is called a 'SEXP'.
--
-- To allow for precise typing of bindings to primitive R functions, we index
-- 'SEXP's by 'SEXPTYPE', which classifies the /form/ of a 'SEXP' (see
-- "Foreign.R.Type").
--
-- Bindings to R functions that allocate or are blocking use safe ccall's, so
-- garbage collection of the Haskell heap can happen concurrently. See the
-- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#foreign-imports-and-multi-threading
-- GHC User's Guide> for more.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Warns about some sanity checks like IsVector, that has no methods and are
-- not used.
{-# OPTIONS_GHC -fno-warn-redundant-constraints -fplugin-opt=LiquidHaskell:--skip-module=False #-}

module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXP(..)
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
  , mkCharLenCE
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
  -- * Internal types and functions
  --
  -- | Should not be used in user code. These exports are only needed for
  -- binding generation tools.
  , SEXPREC
  , SEXP0(..)
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
import Foreign (Ptr, castPtr)
import Foreign.C
import Foreign.R.Context (rCtx, SEXP0(..), SEXPREC)
import Foreign.R.Encoding
import qualified Language.C.Inline as C
-- Use unsafe only for non-blocking, non-allocating functions.
import qualified Language.C.Inline.Unsafe as CU
import Prelude hiding (asTypeOf, length)

C.context (C.baseCtx <> rCtx)
C.include "<Rinternals.h>"
C.include "<stdlib.h>"
C.include "<stdint.h>"

{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--max-case-expand=0" @-}

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | read CAR object value
car :: SEXP s -> IO (SEXP s)
car (unsexp -> s) = sexp <$> [CU.exp| SEXP { CAR( $(SEXP s) ) } |]

{-@ assume setCar :: SEXP s -> b:SEXP s -> IO ({c:SEXP s | c == b}) @-}
-- | Set the CAR value and return it.
setCar :: SEXP s -> SEXP s -> IO (SEXP s)
setCar (unsexp -> s) (unsexp -> s') = sexp <$> [CU.exp| SEXP { SETCAR( $(SEXP s), $(SEXP s') ) } |]

-- | read CDR object
cdr :: SEXP s -> IO (SEXP s)
cdr (unsexp -> s) = sexp <$> [CU.exp| SEXP { CAR( $(SEXP s) ) } |]

{-@ assume setCdr :: SEXP s -> b:SEXP s -> IO ({c:SEXP s | c == b}) @-}
-- | Set the CDR value and return it.
setCdr :: SEXP s -> SEXP s -> IO (SEXP s)
setCdr (unsexp -> s) (unsexp -> s') = sexp <$> [CU.exp| SEXP { SETCDR( $(SEXP s), $(SEXP s') ) } |]

-- | read object`s Tag
tag :: SEXP s -> IO (SEXP s)
tag (unsexp -> s) = sexp <$> [CU.exp| SEXP { TAG( $(SEXP s) ) } |]

setTag :: SEXP s -> SEXP s -> IO ()
setTag (unsexp -> s) (unsexp -> s') = [CU.exp| void { SET_TAG( $(SEXP s), $(SEXP s') ) } |]

--------------------------------------------------------------------------------
-- Environment functions                                                      --
--------------------------------------------------------------------------------

{-@ assume envFrame :: TSEXP s Env -> IO (TSEXP s List) @-}
envFrame :: SEXP s -> IO (SEXP s)
envFrame (unsexp -> s) = sexp <$> [CU.exp| SEXP { FRAME( $(SEXP s) ) } |]

{-@ assume envEnclosing :: TSEXP s Env -> IO (TSEXP s Env) @-}
-- | Enclosing environment.
envEnclosing :: SEXP s -> IO (SEXP s)
envEnclosing (unsexp -> s) = sexp <$> [CU.exp| SEXP { ENCLOS( $(SEXP s) ) } |]

{-@ assume envHashtab :: TSEXP s Env -> IO (TSEXP s Vector) @-}
-- | Hash table associated with the environment, used for faster name lookups.
envHashtab :: SEXP s -> IO (SEXP s)
envHashtab (unsexp -> s) = sexp <$> [CU.exp| SEXP { HASHTAB( $(SEXP s) ) } |]

--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

{-@ assume closureFormals :: TSEXP s Closure -> IO (TSEXP s List) @-}
-- | Closure formals (aka the actual arguments).
closureFormals :: SEXP s -> IO (SEXP s)
closureFormals (unsexp -> s) = sexp <$> [CU.exp| SEXP { FORMALS( $(SEXP s) ) }|]

{-@ assume closureBody :: TSEXP s Closure -> IO (SEXP s) @-}
-- | The code of the closure.
closureBody :: SEXP s -> IO (SEXP s)
closureBody (unsexp -> s) = sexp <$> [CU.exp| SEXP { BODY( $(SEXP s) ) } |]

{-@ assume closureEnv :: TSEXP s Closure -> IO (TSEXP s Env) @-}
-- | The environment of the closure.
closureEnv :: SEXP s -> IO (SEXP s)
closureEnv (unsexp -> s) = sexp <$> [CU.exp| SEXP { CLOENV( $(SEXP s) ) }|]

--------------------------------------------------------------------------------
-- Promise functions                                                          --
--------------------------------------------------------------------------------

{-@ assume promiseCode :: TSEXP s Promise -> IO (SEXP s) @-}
-- | The code of a promise.
promiseCode :: SEXP s -> IO (SEXP s)
promiseCode (unsexp -> s) = sexp <$> [CU.exp| SEXP { PRCODE( $(SEXP s) )}|]

{-@ assume promiseEnv :: TSEXP s Promise -> IO (TSEXP s Env) @-}
-- | The environment in which to evaluate the promise.
promiseEnv :: SEXP s -> IO (SEXP s)
promiseEnv (unsexp -> s) = sexp <$> [CU.exp| SEXP { PRENV( $(SEXP s) )}|]

{-@ assume promiseValue :: TSEXP s Promise -> IO (SEXP s) @-}
-- | The value of the promise, if it has already been forced.
promiseValue :: SEXP s -> IO (SEXP s)
promiseValue (unsexp -> s) = sexp <$> [CU.exp| SEXP { PRVALUE( $(SEXP s) )}|]

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

{-@ reflect _isVector @-}
_isVector :: SEXPTYPE -> Bool
_isVector = \case
  Char -> True
  Logical -> True
  Int -> True
  Real -> True
  Complex -> True
  String -> True
  Vector -> True
  Expr -> True
  WeakRef -> True
  Raw -> True
  _ -> False

{-@ reflect _isGenericVector @-}
_isGenericVector :: SEXPTYPE -> Bool
_isGenericVector = \case
  Vector -> True
  Expr -> True
  WeakRef -> True
  _ -> False

{-@ type VECTORSEXP s = {a:SEXP s | _isVector (typeOf a)} @-}
{-@ type GVECTORSEXP s = {a:SEXP s | _isGenericVector (typeOf a)} @-}

{-@ assume length :: VECTORSEXP s -> IO CInt @-}
-- | Length of the vector.
length :: SEXP s -> IO CInt
length (unsexp -> s) = [CU.exp| int { LENGTH( $(SEXP s) ) }|]

{-@ assume trueLength :: VECTORSEXP s -> IO CInt @-}
-- | Read True Length vector field.
trueLength :: SEXP s -> IO CInt
trueLength (unsexp -> s) = [CU.exp| int { TRUELENGTH( $(SEXP s) ) }|]

{-@ ignore char @-}
{-@ assume char :: TSEXP s Char -> IO CString @-}
-- | Read character vector data
char :: SEXP s -> IO CString
char (unsexp -> s) = castPtr <$> [CU.exp| const char* { CHAR($(SEXP s))}|]
-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

{-@ ignore real @-}
{-@ assume real :: TSEXP s Real -> IO (Ptr Double) @-}
-- | Read real vector data.
real :: SEXP s -> IO (Ptr Double)
real (unsexp -> s) = castPtr <$> [CU.exp| double* { REAL( $(SEXP s)) }|]

{-@ assume integer :: TSEXP s Int -> IO (Ptr Int32) @-}
-- | Read integer vector data.
integer :: SEXP s -> IO (Ptr Int32)
integer (unsexp -> s) = [CU.exp| int32_t* { INTEGER( $(SEXP s) )}|]

{-@ assume raw :: TSEXP s Raw -> IO (Ptr CChar) @-}
-- | Read raw data.
raw :: SEXP s -> IO (Ptr CChar)
raw (unsexp -> s) = [CU.exp| char* { RAW($(SEXP s)) } |]

{-@ ignore logical @-}
{-@ assume logical :: TSEXP s Logical -> IO (Ptr Logical) @-}
-- | Read logical vector data.
logical :: SEXP s -> IO (Ptr R.Logical)
logical (unsexp -> s) = castPtr <$>
  [CU.exp| int* { LOGICAL($(SEXP s)) } |]

{-@ assume complex :: TSEXP s Complex -> IO (Ptr (Complex Double)) @-}
-- | Read complex vector data.
complex :: SEXP s -> IO (Ptr (Complex Double))
complex (unsexp -> s) = [CU.exp| Rcomplex* { COMPLEX($(SEXP s)) }|]

{-@ ignore string @-}
{-@ assume string :: TSEXP s String -> IO (Ptr (TSEXP s Char)) @-}
-- | Read string vector data.
string :: SEXP s -> IO (Ptr (SEXP s))
string (unsexp -> s) = castPtr <$>
  [CU.exp| SEXP* { STRING_PTR($(SEXP s)) }|]

{-@ assume readVector :: GVECTORSEXP s -> Int -> IO (SEXP s) @-}
readVector :: SEXP s -> Int -> IO (SEXP s)
readVector (unsexp -> s) (fromIntegral -> n) = sexp <$>
  [CU.exp| SEXP { VECTOR_ELT( $(SEXP s), $(int n) ) } |]

{-@ indexVector :: GVECTORSEXP s -> Int -> IO (SEXP s) @-}
indexVector :: SEXP s -> Int -> IO (SEXP s)
{-# DEPRECATED indexVector "Use readVector instead." #-}
indexVector = readVector

{-@ assume writeVector :: GVECTORSEXP s -> Int -> SEXP s -> IO (GVECTORSEXP s) @-}
writeVector :: SEXP s -> Int -> SEXP s -> IO (SEXP s)
writeVector (unsexp -> a) (fromIntegral -> n) (unsexp -> b) = sexp <$>
  [CU.exp| SEXP { SET_VECTOR_ELT($(SEXP a),$(int n), $(SEXP b)) } |]

-- | Extract the data pointer from a vector.
unsafeSEXPToVectorPtr :: SEXP s -> Ptr ()
unsafeSEXPToVectorPtr (unsexp -> s) =
  [C.pure| void * { DATAPTR( $(SEXP s) ) } |]

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

{-@ assume symbolPrintName :: TSEXP s Symbol -> IO (SEXP s) @-}
-- | Read a name from symbol.
symbolPrintName :: SEXP s -> IO (SEXP s)
symbolPrintName (unsexp -> s) = sexp <$> [CU.exp| SEXP { PRINTNAME( $(SEXP s)) } |]

{-@ assume symbolValue :: TSEXP s Symbol -> IO (SEXP s) @-}
-- | Read value from symbol.
symbolValue :: SEXP s -> IO (SEXP s)
symbolValue (unsexp -> s) = sexp <$> [CU.exp| SEXP { SYMVALUE( $(SEXP s)) } |]

{-@ assume symbolInternal :: TSEXP s Symbol -> IO (SEXP s) @-}
-- | Read internal value from symbol.
symbolInternal :: SEXP s -> IO (SEXP s)
symbolInternal (unsexp -> s) = sexp <$> [CU.exp| SEXP { INTERNAL( $(SEXP s)) }|]

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

{-@ assume mkString :: CString -> IO (TSEXP V String) @-}
-- | Initialize a new string vector.
mkString :: CString -> IO (SEXP V)
mkString value = sexp <$>  [C.exp| SEXP { Rf_mkString($(char * value)) } |]

{-@ assume mkChar :: CString -> IO (TSEXP V Char) @-}
-- | Initialize a new character vector (aka a string).
mkChar :: CString -> IO (SEXP V)
mkChar value = sexp <$> [C.exp| SEXP { Rf_mkChar($(char * value)) } |]

{-@ assume mkCharCE :: CEType -> CString -> IO (TSEXP V Char) @-}
-- | Create Character value with specified encoding
mkCharCE :: CEType -> CString -> IO (SEXP V)
mkCharCE (cIntFromEnum -> ce) value = sexp <$> 
  [C.exp| SEXP  { Rf_mkCharCE($(char * value), $(int ce)) } |]

{-@ assume mkCharLenCE :: CEType -> CString -> Int -> IO (TSEXP V Char) @-}
mkCharLenCE :: CEType -> CString -> Int -> IO (SEXP V)
mkCharLenCE (cIntFromEnum -> ce) value (fromIntegral -> len) = sexp <$>
  [C.exp| SEXP { Rf_mkCharLenCE($(char * value), $(int len), $(int ce)) } |]

{-@ assume install :: CString -> IO (TSEXP V Symbol) @-}
-- | Intern a string @name@ into the symbol table.
--
-- If @name@ is not found, it is added to the symbol table. The symbol
-- corresponding to the string @name@ is returned.
install :: CString -> IO (SEXP V)
install name = sexp <$>
  [C.exp| SEXP { Rf_install($(char * name)) }|]

{-@ assume allocSEXP :: x:SEXPTYPE -> IO (TSEXP V x) @-}
-- | Allocate a 'SEXP'.
allocSEXP :: SEXPTYPE -> IO (SEXP V)
allocSEXP (fromIntegral . fromEnum -> s) = sexp <$>
  [C.exp| SEXP { Rf_allocSExp( $(unsigned int s) ) }|]

{-@ assume allocList :: Int -> IO (TSEXP V List) @-}
-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: Int -> IO (SEXP V)
allocList (fromIntegral -> n) = sexp <$> [C.exp| SEXP {Rf_allocList($(int n))} |]

{-@ assume allocVector :: {x:SEXPTYPE | _isVector x} -> Int -> IO (TSEXP V x) @-}
-- | Allocate Vector.
allocVector :: SEXPTYPE -> Int -> IO (SEXP V)
allocVector (fromIntegral . fromEnum -> p) (fromIntegral -> n) = sexp <$>
  [C.exp| SEXP {Rf_allocVector( $(unsigned int p), $(int n)) } |]

{-@ allocVectorProtected :: {x:SEXPTYPE | _isVector x} -> Int -> IO (TSEXP s x) @-}
allocVectorProtected :: SEXPTYPE -> Int -> IO (SEXP s)
allocVectorProtected ty n = fmap release (protect =<< allocVector ty n)

{-@ assume cons :: SEXP s -> SEXP s -> IO (TSEXP V List) @-}
-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: SEXP s -> SEXP s -> IO (SEXP V)
cons (unsexp -> a) (unsexp -> b) = sexp <$>
  [C.exp| SEXP { Rf_cons($(SEXP a), $(SEXP b)) }|]

{-@ assume lcons :: SEXP s -> SEXP s -> IO (TSEXP V Lang) @-}
-- | Allocate a so-called cons cell of language objects, in essence a pair of
-- 'SEXP' pointers.
lcons :: SEXP s -> SEXP s -> IO (SEXP V)
lcons (unsexp -> a) (unsexp -> b) = sexp <$>
  [C.exp| SEXP { Rf_lcons($(SEXP a), $(SEXP b)) } |]


printValue :: SEXP s -> IO ()
printValue (unsexp -> s) =
  [C.exp| void { Rf_PrintValue($(SEXP s)) }|]

--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------

{-@ assume protect :: a:SEXP s -> IO (TSEXP G (Foreign.R.Internal.typeOf a)) @-}
-- | Protect a 'SEXP' from being garbage collected by R. It is in particular
-- necessary to do so for objects that are not yet pointed by any other object,
-- e.g. when constructing a tree bottom-up rather than top-down.
--
-- To avoid unbalancing calls to 'protect' and 'unprotect', do not use these
-- functions directly but use 'Language.R.withProtected' instead.
protect :: SEXP s -> IO (SEXP G)
protect (unsexp -> s) = sexp <$> 
  [CU.exp| SEXP { Rf_protect($(SEXP s)) }|]

-- | @unprotect n@ unprotects the last @n@ objects that were protected.
unprotect :: Int -> IO ()
unprotect (fromIntegral -> i) =
  [CU.exp| void { Rf_unprotect($(int i)) } |]

-- | Unprotect a specific object, referred to by pointer.
unprotectPtr :: SEXP G -> IO ()
unprotectPtr (unsexp -> s) =
  [CU.exp| void { Rf_unprotect_ptr($(SEXP s)) }|]

-- | Invoke an R garbage collector sweep.
gc :: IO ()
gc = [C.exp| void { R_gc() }|]

-- | Preserve an object accross GCs.
preserveObject :: SEXP s -> IO ()
preserveObject (unsexp -> s) =
  [CU.exp| void { R_PreserveObject( $(SEXP s) )} |]

-- | Allow GC to remove an preserved object.
releaseObject :: SEXP s -> IO ()
releaseObject (unsexp -> s) =
  [CU.exp| void { R_ReleaseObject( $(SEXP s) )} |]

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

{-@ assume eval :: SEXP s -> TSEXP s Env -> IO (SEXP V) @-}
-- | Evaluate any 'SEXP' to its value.
eval :: SEXP s -> SEXP s -> IO (SEXP V)
eval (unsexp -> expr) (unsexp -> env) = sexp <$>
  [C.exp| SEXP { Rf_eval($(SEXP expr), $(SEXP env)) }|]

{-@ assume tryEval :: SEXP s -> TSEXP s Env -> Ptr CInt -> IO (SEXP V) @-}
-- | Try to evaluate expression.
tryEval :: SEXP s -> SEXP s -> Ptr CInt -> IO (SEXP V)
tryEval (unsexp -> expr) (unsexp -> env) retCode = sexp <$>
  [C.exp| SEXP { R_tryEval($(SEXP expr), $(SEXP env), $(int* retCode)) }|]

{-@ assume tryEvalSilent :: SEXP s -> TSEXP s Env -> Ptr CInt -> IO (SEXP V) @-}
-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: SEXP  s -> SEXP s -> Ptr CInt -> IO (SEXP V)
tryEvalSilent (unsexp -> expr) (unsexp -> env) retCode = sexp <$>
  [C.exp| SEXP { R_tryEvalSilent($(SEXP expr), $(SEXP env), $(int* retCode)) }|]

{-@ assume lang1 :: SEXP s -> IO (TSEXP V Lang) @-}
-- | Construct a nullary function call.
lang1 :: SEXP s -> IO (SEXP V)
lang1 (unsexp -> s) = sexp <$>
  [C.exp| SEXP {Rf_lang1($(SEXP s)) }|]

{-@ assume lang2 :: SEXP s -> SEXP s -> IO (TSEXP V Lang) @-}
-- | Construct unary function call.
lang2 :: SEXP s -> SEXP s ->  IO (SEXP V)
lang2 (unsexp -> f) (unsexp -> x) = sexp <$>
  [C.exp| SEXP {Rf_lang2($(SEXP f), $(SEXP x)) }|]

{-@ assume lang3 :: SEXP s -> SEXP s -> SEXP s -> IO (TSEXP V Lang) @-}
-- | Construct a binary function call.
lang3 :: SEXP s -> SEXP s ->  SEXP s -> IO (SEXP V)
lang3 (unsexp -> f) (unsexp -> x) (unsexp -> y) = sexp <$>
  [C.exp| SEXP {Rf_lang3($(SEXP f), $(SEXP x), $(SEXP y)) }|]

{-@ assume findFun :: SEXP s -> TSEXP s Env -> IO (SEXP s) @-}
-- | Find a function by name.
findFun :: SEXP s -> SEXP s -> IO (SEXP s)
findFun (unsexp -> a) (unsexp -> env) = sexp <$>
  [CU.exp| SEXP { Rf_findFun($(SEXP a), $(SEXP env)) }|]

{-@ assume findVar :: SEXP s -> TSEXP s Env -> IO (TSEXP s Symbol) @-}
-- | Find a variable by name.
findVar :: SEXP s -> SEXP s -> IO (SEXP s)
findVar (unsexp -> a) (unsexp -> env) = sexp <$>
  [CU.exp| SEXP {Rf_findVar($(SEXP a), $(SEXP env))}|]

{-@ assume mkWeakRef :: SEXP s -> SEXP s -> SEXP s -> Bool -> IO (TSEXP V WeakRef) @-}
mkWeakRef :: SEXP s -> SEXP s -> SEXP s -> Bool -> IO (SEXP V)
mkWeakRef (unsexp -> a) (unsexp -> b) (unsexp -> c) (cIntFromEnum -> t) = sexp <$>
  [C.exp| SEXP {R_MakeWeakRef($(SEXP a), $(SEXP b), $(SEXP c), $(int t))}|]

{-@ assume withProtected :: forall <p :: SEXP s -> Bool > . IO ((SEXP V)<p>) -> ((SEXP s)<p> -> IO b) -> IO b @-}
-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: IO (SEXP V)      -- Action to acquire resource
              -> (SEXP s -> IO b) -- Action
              -> IO b
withProtected create f =
    bracket
      (do { x <- create; _ <- protect x; return x })
      (const $ unprotect 1)
      (f . unsafeRelease)
