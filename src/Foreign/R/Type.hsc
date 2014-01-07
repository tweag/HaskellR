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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Foreign.R.Type where

#include <Rinternals.h>

import H.Constraints
import H.Internal.Error

import qualified Language.Haskell.TH.Syntax as Hs
import qualified Language.Haskell.TH.Lib as Hs

import Data.Singletons.TH

import Foreign (castPtr)
import Foreign.C (CInt)
import Foreign.Storable(Storable(..))
import Prelude hiding (Bool(..))

-- | R "type". Note that what R calls a "type" is not what is usually meant by
-- the term: there is really only a single type, called 'SEXP', and an R "type"
-- in fact refers to the /class/ or /form/ of the expression.
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
    deriving (Eq, Show)

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

genSingletons [''SEXPTYPE]

instance Hs.Lift SEXPTYPE where
  lift a = [| $(Hs.conE (Hs.mkName $ "Foreign.R.Type." ++ show a)) |]

-- | R uses three-valued logic.
data Logical = False
             | True
             | NA
-- XXX no Enum instance because NA = INT_MIN, not representable as an Int on
-- 32-bit systems.
               deriving (Eq, Show)

instance Storable Logical where
  sizeOf _       = sizeOf (undefined :: CInt)
  alignment _    = alignment (undefined :: CInt)
  poke ptr False = poke (castPtr ptr) (0 :: CInt)
  poke ptr True  = poke (castPtr ptr) (1 :: CInt)
  -- Currently NA_LOGICAL = INT_MIN.
  poke ptr NA    = poke (castPtr ptr) (#{const INT_MIN} :: CInt)
  peek ptr = do
      x <- peek (castPtr ptr)
      case x :: CInt of
          0 -> return False
          1 -> return True
          #{const INT_MIN} -> return NA
          _ -> failure "Storable Logical peek" "Not a Logical."

-- | Used where the R documentation speaks of "pairlists", which are really just
-- regular lists.
type PairList = List

#let VECTOR_FORMS = " 'Char \
                  :+: 'Logical \
                  :+: 'Int \
                  :+: 'Real \
                  :+: 'Complex \
                  :+: 'String \
                  :+: 'Vector \
                  :+: 'Expr \
                  :+: 'WeakRef \
                  :+: 'Raw"

-- | Constraint synonym grouping all vector forms into one class. @IsVector a@
-- holds iff R's @is.vector()@ returns @TRUE@.
type IsVector (a :: SEXPTYPE) = a :∈ #{VECTOR_FORMS}

-- | Non-atomic vector forms. See @src/main/memory.c:SET_VECTOR_ELT@ in R source.
type IsGenericVector (a :: SEXPTYPE) = a :∈ Vector :+: Expr :+: WeakRef

-- | @IsList a@ holds iff R's @is.vector()@ returns @TRUE@.
type IsList (a :: SEXPTYPE) = a :∈ #{VECTOR_FORMS} :+: List
