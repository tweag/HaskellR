{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Definition of 'SEXPTYPE', which classifies the possible forms of an
-- R expression (a 'SEXP'). It is normally not necessary to import this module
-- directly, since it is reexported by "Foreign.R".
--
-- This is done in a separate module because we want to use hsc2hs rather than
-- c2hs for discharging the boilerplate around 'SEXPTYPE'. This is because
-- 'SEXPTYPE' is nearly but not quite a true enumeration and c2hs has trouble
-- dealing with that.
--
-- This module also defines a singleton version of 'SEXPTYPE', called
-- 'SSEXPTYPE'. This is actually a family of types, one for each possible
-- 'SEXPTYPE'. Singleton types are a way of emulating dependent types in
-- a language that does not have true dependent type. They are useful in
-- functions whose result type depends on the value of one of its arguments. See
-- e.g. 'Foreign.R.allocVector'.

module Foreign.R.Type
  ( SEXPTYPE(..)
  , SSEXPTYPE(..)
  , Sing(..)
  , Logical(..)
  , PairList
  , IsVector
  , IsGenericVector
  , IsList
  , IsPairList
  , IsExpression
  ) where

#include <Rinternals.h>

import Foreign.R.Constraints
import Internal.Error

import qualified Language.Haskell.TH.Syntax as Hs
import qualified Language.Haskell.TH.Lib as Hs

import Data.Singletons.TH

import Control.DeepSeq (NFData(..))
import Foreign.R.Context

-- | R \"type\". Note that what R calls a \"type\" is not what is usually meant
-- by the term: there is really only a single type, called 'SEXP', and an
-- R "type" in fact refers to the /class/ or /form/ of the expression.
--
-- To better illustrate the distinction, note that any sane type system normally
-- has the /subject reduction property/: that the type of an expression is
-- invariant under reduction. For example, @(\x -> x) 1@ has type 'Int', and so
-- does the value of this expression, @2@, have type 'Int'. Yet the /form/ of
-- the expression is an application of a function to a literal, while the form
-- of its reduct is an integer literal.
--
-- We introduce convenient Haskell-like names for forms because this datatype is
-- used to index 'SEXP' and other types through the @DataKinds@ extension.
--
data SEXPTYPE
    = Nil
    | Symbol
    | List
    | Closure
    | Env
    | Promise
    | Lang
    | Special
    | Builtin
    | Char
    | Logical
    | Int
    | Real
    | Complex
    | String
    | DotDotDot
    | Any
    | Vector
    | Expr
    | Bytecode
    | ExtPtr
    | WeakRef
    | Raw
    | S4
    | New
    | Free
    | Fun
    deriving (Eq, Ord, Show)

instance Enum SEXPTYPE where
  fromEnum Nil        = #const NILSXP
  fromEnum Symbol     = #const SYMSXP
  fromEnum List       = #const LISTSXP
  fromEnum Closure    = #const CLOSXP
  fromEnum Env        = #const ENVSXP
  fromEnum Promise    = #const PROMSXP
  fromEnum Lang       = #const LANGSXP
  fromEnum Special    = #const SPECIALSXP
  fromEnum Builtin    = #const BUILTINSXP
  fromEnum Char       = #const CHARSXP
  fromEnum Logical    = #const LGLSXP
  fromEnum Int        = #const INTSXP
  fromEnum Real       = #const REALSXP
  fromEnum Complex    = #const CPLXSXP
  fromEnum String     = #const STRSXP
  fromEnum DotDotDot  = #const DOTSXP
  fromEnum Any        = #const ANYSXP
  fromEnum Vector     = #const VECSXP
  fromEnum Expr       = #const EXPRSXP
  fromEnum Bytecode   = #const BCODESXP
  fromEnum ExtPtr     = #const EXTPTRSXP
  fromEnum WeakRef    = #const WEAKREFSXP
  fromEnum Raw        = #const RAWSXP
  fromEnum S4         = #const S4SXP
  fromEnum New        = #const NEWSXP
  fromEnum Free       = #const FREESXP
  fromEnum Fun        = #const FUNSXP

  toEnum (#const NILSXP)     = Nil
  toEnum (#const SYMSXP)     = Symbol
  toEnum (#const LISTSXP)    = List
  toEnum (#const CLOSXP)     = Closure
  toEnum (#const ENVSXP)     = Env
  toEnum (#const PROMSXP)    = Promise
  toEnum (#const LANGSXP)    = Lang
  toEnum (#const SPECIALSXP) = Special
  toEnum (#const BUILTINSXP) = Builtin
  toEnum (#const CHARSXP)    = Char
  toEnum (#const LGLSXP)     = Logical
  toEnum (#const INTSXP)     = Int
  toEnum (#const REALSXP)    = Real
  toEnum (#const CPLXSXP)    = Complex
  toEnum (#const STRSXP)     = String
  toEnum (#const DOTSXP)     = DotDotDot
  toEnum (#const ANYSXP)     = Any
  toEnum (#const VECSXP)     = Vector
  toEnum (#const EXPRSXP)    = Expr
  toEnum (#const BCODESXP)   = Bytecode
  toEnum (#const EXTPTRSXP)  = ExtPtr
  toEnum (#const WEAKREFSXP) = WeakRef
  toEnum (#const RAWSXP)     = Raw
  toEnum (#const S4SXP)      = S4
  toEnum (#const NEWSXP)     = New
  toEnum (#const FREESXP)    = Free
  toEnum (#const FUNSXP)     = Fun
  toEnum _                   = violation "toEnum" "Unknown R type."

instance NFData SEXPTYPE where
  rnf = (`seq` ())

genSingletons [''SEXPTYPE]

instance Hs.Lift SEXPTYPE where
  lift a = [| $(Hs.conE (Hs.mkName $ "Foreign.R.Type." ++ show a)) |]

-- | Used where the R documentation speaks of "pairlists", which are really just
-- regular lists.
type PairList = List

-- Use a macro to avoid having to define append at the type level.
#let VECTOR_FORMS = " 'Char \
                   ': 'Logical \
                   ': 'Int \
                   ': 'Real \
                   ': 'Complex \
                   ': 'String \
                   ': 'Vector \
                   ': 'Expr \
                   ': 'WeakRef \
                   ': 'Raw"

-- | Constraint synonym grouping all vector forms into one class. @IsVector a@
-- holds iff R's @is.vector()@ returns @TRUE@.
type IsVector (a :: SEXPTYPE) = (SingI a, a :∈ #{VECTOR_FORMS} ': '[])

-- | Non-atomic vector forms. See @src\/main\/memory.c:SET_VECTOR_ELT@ in the
-- R source distribution.
type IsGenericVector (a :: SEXPTYPE) = (SingI a, a :∈ [Vector, Expr, WeakRef])

-- | @IsList a@ holds iff R's @is.list()@ returns @TRUE@.
type IsList (a :: SEXPTYPE) = (SingI a, a :∈ #{VECTOR_FORMS} ': List ': '[])

-- | @IsPairList a@ holds iff R's @is.pairlist()@ returns @TRUE@.
type IsPairList (a :: SEXPTYPE) = (SingI a, a :∈ [List, Nil])

-- | Constraint synonym grouping all expression forms into one class. According
-- to R internals, an expression is usually a 'Lang', but can sometimes also be
-- an 'Expr' or a 'Symbol'.
type IsExpression (a :: SEXPTYPE) = (SingI a, a :∈ [Lang, Expr, Symbol])