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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , unsafeReleaseSome
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

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | read CAR object value
car :: SEXP s a -> IO (SomeSEXP s)
car (unsexp -> s) = somesexp <$> [CU.exp| SEXP { CAR( $(SEXP s) ) } |]

-- | Set the CAR value and return it.
setCar :: SEXP s a -> SEXP s b -> IO (SEXP s b)
setCar (unsexp -> s) (unsexp -> s') = sexp <$> [CU.exp| SEXP { SETCAR( $(SEXP s), $(SEXP s') ) } |]

-- | read CDR object
cdr :: SEXP s a -> IO (SomeSEXP s)
cdr (unsexp -> s) = somesexp <$> [CU.exp| SEXP { CAR( $(SEXP s) ) } |]

-- | Set the CDR value and return it.
setCdr :: SEXP s a -> SEXP s b -> IO (SEXP s b)
setCdr (unsexp -> s) (unsexp -> s') = sexp <$> [CU.exp| SEXP { SETCDR( $(SEXP s), $(SEXP s') ) } |]

-- | read object`s Tag
tag :: SEXP s a -> IO (SomeSEXP s)
tag (unsexp -> s) = somesexp <$> [CU.exp| SEXP { TAG( $(SEXP s) ) } |]

setTag :: SEXP s a -> SEXP s b -> IO ()
setTag (unsexp -> s) (unsexp -> s') = [CU.exp| void { SET_TAG( $(SEXP s), $(SEXP s') ) } |]

--------------------------------------------------------------------------------
-- Environment functions                                                      --
--------------------------------------------------------------------------------

envFrame :: (SEXP s 'R.Env) -> IO (SEXP s R.PairList)
envFrame (unsexp -> s) = sexp <$> [CU.exp| SEXP { FRAME( $(SEXP s) ) } |]

-- | Enclosing environment.
envEnclosing :: SEXP s 'R.Env -> IO (SEXP s 'R.Env)
envEnclosing (unsexp -> s) = sexp <$> [CU.exp| SEXP { ENCLOS( $(SEXP s) ) } |]

-- | Hash table associated with the environment, used for faster name lookups.
envHashtab :: SEXP s 'R.Env -> IO (SEXP s 'R.Vector)
envHashtab (unsexp -> s) = sexp <$> [CU.exp| SEXP { HASHTAB( $(SEXP s) ) } |]

--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

-- | Closure formals (aka the actual arguments).
closureFormals :: SEXP s 'R.Closure -> IO (SEXP s R.PairList)
closureFormals (unsexp -> s) = sexp <$> [CU.exp| SEXP { FORMALS( $(SEXP s) ) }|]

-- | The code of the closure.
closureBody :: SEXP s 'R.Closure -> IO (SomeSEXP s)
closureBody (unsexp -> s) = somesexp <$> [CU.exp| SEXP { BODY( $(SEXP s) ) } |]

-- | The environment of the closure.
closureEnv :: SEXP s 'R.Closure -> IO (SEXP s 'R.Env)
closureEnv (unsexp -> s) = sexp <$> [CU.exp| SEXP { CLOENV( $(SEXP s) ) }|]

--------------------------------------------------------------------------------
-- Promise functions                                                          --
--------------------------------------------------------------------------------

-- | The code of a promise.
promiseCode :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseCode (unsexp -> s) = somesexp <$> [CU.exp| SEXP { PRCODE( $(SEXP s) )}|]

-- | The environment in which to evaluate the promise.
promiseEnv :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseEnv (unsexp -> s) = somesexp <$> [CU.exp| SEXP { PRENV( $(SEXP s) )}|]

-- | The value of the promise, if it has already been forced.
promiseValue :: SEXP s 'R.Promise -> IO (SomeSEXP s)
promiseValue (unsexp -> s) = somesexp <$> [CU.exp| SEXP { PRVALUE( $(SEXP s) )}|]

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
length :: R.IsVector a => SEXP s a -> IO CInt
length (unsexp -> s) = [CU.exp| int { LENGTH( $(SEXP s) ) }|]

-- | Read True Length vector field.
trueLength :: R.IsVector a => SEXP s a -> IO CInt
trueLength (unsexp -> s) = [CU.exp| int { TRUELENGTH( $(SEXP s) ) }|]

-- | Read character vector data
char :: SEXP s 'R.Char -> IO CString
char (unsexp -> s) = castPtr <$> [CU.exp| const char* { CHAR($(SEXP s))}|]
-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

-- | Read real vector data.
real :: SEXP s 'R.Real -> IO (Ptr Double)
real (unsexp -> s) = castPtr <$> [CU.exp| double* { REAL( $(SEXP s)) }|]

-- | Read integer vector data.
integer :: SEXP s 'R.Int -> IO (Ptr Int32)
integer (unsexp -> s) = [CU.exp| int32_t* { INTEGER( $(SEXP s) )}|]

-- | Read raw data.
raw :: SEXP s 'R.Raw -> IO (Ptr CChar)
raw (unsexp -> s) = [CU.exp| char* { RAW($(SEXP s)) } |]

-- | Read logical vector data.
logical :: SEXP s 'R.Logical -> IO (Ptr R.Logical)
logical (unsexp -> s) = castPtr <$>
  [CU.exp| int* { LOGICAL($(SEXP s)) } |]

-- | Read complex vector data.
complex :: SEXP s 'R.Complex -> IO (Ptr (Complex Double))
complex (unsexp -> s) = [CU.exp| Rcomplex* { COMPLEX($(SEXP s)) }|]

-- | Read string vector data.
string :: SEXP s 'R.String -> IO (Ptr (SEXP s 'R.Char))
string (unsexp -> s) = castPtr <$>
  [CU.exp| SEXP* { STRING_PTR($(SEXP s)) }|]

readVector :: R.IsGenericVector a => SEXP s a -> Int -> IO (SomeSEXP s)
readVector (unsexp -> s) (fromIntegral -> n) = somesexp <$>
  [CU.exp| SEXP { VECTOR_ELT( $(SEXP s), $(int n) ) } |]

indexVector :: IsGenericVector a => SEXP s a -> Int -> IO (SomeSEXP s)
{-# DEPRECATED indexVector "Use readVector instead." #-}
indexVector = readVector

writeVector :: R.IsGenericVector a => SEXP s a -> Int -> SEXP s b -> IO (SEXP s a)
writeVector (unsexp -> a) (fromIntegral -> n) (unsexp -> b) = sexp <$>
  [CU.exp| SEXP { SET_VECTOR_ELT($(SEXP a),$(int n), $(SEXP b)) } |]

-- | Extract the data pointer from a vector.
unsafeSEXPToVectorPtr :: SEXP s a -> Ptr ()
unsafeSEXPToVectorPtr (unsexp -> s) =
  [C.pure| void * { DATAPTR( $(SEXP s) ) } |]

--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
symbolPrintName :: SEXP s 'R.Symbol -> IO (SomeSEXP s)
symbolPrintName (unsexp -> s) = somesexp <$> [CU.exp| SEXP { PRINTNAME( $(SEXP s)) } |]

-- | Read value from symbol.
symbolValue :: SEXP s 'R.Symbol -> IO (SomeSEXP s)
symbolValue (unsexp -> s) = somesexp <$> [CU.exp| SEXP { SYMVALUE( $(SEXP s)) } |]

-- | Read internal value from symbol.
symbolInternal :: SEXP s 'R.Symbol -> IO (SomeSEXP s)
symbolInternal (unsexp -> s) = somesexp <$> [CU.exp| SEXP { INTERNAL( $(SEXP s)) }|]

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Initialize a new string vector.
mkString :: CString -> IO (SEXP V 'R.String)
mkString value = sexp <$>  [C.exp| SEXP { Rf_mkString($(char * value)) } |]

-- | Initialize a new character vector (aka a string).
mkChar :: CString -> IO (SEXP V 'R.Char)
mkChar value = sexp <$> [C.exp| SEXP { Rf_mkChar($(char * value)) } |]

-- | Create Character value with specified encoding
mkCharCE :: CEType -> CString -> IO (SEXP V 'R.Char)
mkCharCE (cIntFromEnum -> ce) value = sexp <$> 
  [C.exp| SEXP  { Rf_mkCharCE($(char * value), $(int ce)) } |]

mkCharLenCE :: CEType -> CString -> Int -> IO (SEXP V 'R.Char)
mkCharLenCE (cIntFromEnum -> ce) value (fromIntegral -> len) = sexp <$>
  [C.exp| SEXP { Rf_mkCharLenCE($(char * value), $(int len), $(int ce)) } |]

-- | Intern a string @name@ into the symbol table.
--
-- If @name@ is not found, it is added to the symbol table. The symbol
-- corresponding to the string @name@ is returned.
install :: CString -> IO (SEXP V 'R.Symbol)
install name = sexp <$>
  [C.exp| SEXP { Rf_install($(char * name)) }|]

-- | Allocate a 'SEXP'.
allocSEXP :: SSEXPTYPE a -> IO (SEXP V a)
allocSEXP (cUIntFromSingEnum -> s) = sexp <$>
  [C.exp| SEXP { Rf_allocSExp( $(unsigned int s) ) }|]

-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: Int -> IO (SEXP V 'R.List)
allocList (fromIntegral -> n) = sexp <$> [C.exp| SEXP {Rf_allocList($(int n))} |]

-- | Allocate Vector.
allocVector :: R.IsVector a => SSEXPTYPE a -> Int -> IO (SEXP V a)
allocVector (cUIntFromSingEnum -> p) (fromIntegral -> n) = sexp <$>
  [C.exp| SEXP {Rf_allocVector( $(unsigned int p), $(int n)) } |]

allocVectorProtected :: (R.IsVector a) => SSEXPTYPE a -> Int -> IO (SEXP s a)
allocVectorProtected ty n = fmap release (protect =<< allocVector ty n)

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: SEXP s a -> SEXP s b -> IO (SEXP V 'R.List)
cons (unsexp -> a) (unsexp -> b) = sexp <$>
  [C.exp| SEXP { Rf_cons($(SEXP a), $(SEXP b)) }|]

-- | Allocate a so-called cons cell of language objects, in essence a pair of
-- 'SEXP' pointers.
lcons :: SEXP s a -> SEXP s b -> IO (SEXP V 'R.Lang)
lcons (unsexp -> a) (unsexp -> b) = sexp <$>
  [C.exp| SEXP { Rf_lcons($(SEXP a), $(SEXP b)) } |]


printValue :: SEXP s a -> IO ()
printValue (unsexp -> s) =
  [C.exp| void { Rf_PrintValue($(SEXP s)) }|]

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
protect (unsexp -> s) = sexp <$> 
  [CU.exp| SEXP { Rf_protect($(SEXP s)) }|]

-- | @unprotect n@ unprotects the last @n@ objects that were protected.
unprotect :: Int -> IO ()
unprotect (fromIntegral -> i) =
  [CU.exp| void { Rf_unprotect($(int i)) } |]

-- | Unprotect a specific object, referred to by pointer.
unprotectPtr :: SEXP G a -> IO ()
unprotectPtr (unsexp -> s) =
  [CU.exp| void { Rf_unprotect_ptr($(SEXP s)) }|]

-- | Invoke an R garbage collector sweep.
gc :: IO ()
gc = [C.exp| void { R_gc() }|]

-- | Preserve an object accross GCs.
preserveObject :: SEXP s a -> IO ()
preserveObject (unsexp -> s) =
  [CU.exp| void { R_PreserveObject( $(SEXP s) )} |]

-- | Allow GC to remove an preserved object.
releaseObject :: SEXP s a -> IO ()
releaseObject (unsexp -> s) =
  [CU.exp| void { R_ReleaseObject( $(SEXP s) )} |]

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
eval :: SEXP s a -> SEXP s 'R.Env -> IO (SomeSEXP V)
eval (unsexp -> expr) (unsexp -> env) = somesexp <$>
  [C.exp| SEXP { Rf_eval($(SEXP expr), $(SEXP env)) }|]

-- | Try to evaluate expression.
tryEval :: SEXP s a -> SEXP s 'R.Env -> Ptr CInt -> IO (SomeSEXP V)
tryEval (unsexp -> expr) (unsexp -> env) retCode = somesexp <$>
  [C.exp| SEXP { R_tryEval($(SEXP expr), $(SEXP env), $(int* retCode)) }|]

-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: SEXP  s a -> SEXP s 'R.Env -> Ptr CInt -> IO (SomeSEXP V)
tryEvalSilent (unsexp -> expr) (unsexp -> env) retCode = somesexp <$>
  [C.exp| SEXP { R_tryEvalSilent($(SEXP expr), $(SEXP env), $(int* retCode)) }|]

-- | Construct a nullary function call.
lang1 :: SEXP s a -> IO (SEXP V 'R.Lang)
lang1 (unsexp -> s) = sexp <$>
  [C.exp| SEXP {Rf_lang1($(SEXP s)) }|]

-- | Construct unary function call.
lang2 :: SEXP s a -> SEXP s b ->  IO (SEXP V 'R.Lang)
lang2 (unsexp -> f) (unsexp -> x) = sexp <$>
  [C.exp| SEXP {Rf_lang2($(SEXP f), $(SEXP x)) }|]

-- | Construct a binary function call.
lang3 :: SEXP s a -> SEXP s b ->  SEXP s c -> IO (SEXP V 'R.Lang)
lang3 (unsexp -> f) (unsexp -> x) (unsexp -> y) = sexp <$>
  [C.exp| SEXP {Rf_lang3($(SEXP f), $(SEXP x), $(SEXP y)) }|]

-- | Find a function by name.
findFun :: SEXP s a -> SEXP s 'R.Env -> IO (SomeSEXP s)
findFun (unsexp -> a) (unsexp -> env) = somesexp <$>
  [CU.exp| SEXP { Rf_findFun($(SEXP a), $(SEXP env)) }|]

-- | Find a variable by name.
findVar :: SEXP s a -> SEXP s 'R.Env -> IO (SEXP s 'R.Symbol)
findVar (unsexp -> a) (unsexp -> env) = sexp <$>
  [CU.exp| SEXP {Rf_findVar($(SEXP a), $(SEXP env))}|]

mkWeakRef :: SEXP s a -> SEXP s b -> SEXP s c -> Bool -> IO (SEXP V 'R.WeakRef)
mkWeakRef (unsexp -> a) (unsexp -> b) (unsexp -> c) (cIntFromEnum -> t) = sexp <$>
  [C.exp| SEXP {R_MakeWeakRef($(SEXP a), $(SEXP b), $(SEXP c), $(int t))}|]

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
