-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.SEXP.Base where

import Control.Memory.Region

import Foreign.R.Type
import Foreign.R.Context -- XXX: needed to help LH name resolution
import Foreign.R.Internal -- XXX: needed to help LH name resolution
import Foreign.R (SEXP)

import Data.Singletons (SingI)

import Data.Complex (Complex)
import Data.Word (Word8)
import Data.Int (Int32)
import Foreign.Storable (Storable)

-- | Function from R types to the types of the representations of each element
-- in the vector.
type family ElemRep s (a :: SEXPTYPE) where
  ElemRep s 'Char    = Word8
  ElemRep s 'Logical = Logical
  ElemRep s 'Int     = Int32
  ElemRep s 'Real    = Double
  ElemRep s 'Complex = Complex Double
  ElemRep s 'String  = SEXP s -- SEXP s 'Char
  ElemRep s 'Vector  = SEXP s
  ElemRep s 'Expr    = SEXP s
  ElemRep s 'Raw     = Word8

-- | 'ElemRep' in the form of a relation, for convenience.
type E s a b = ElemRep s a ~ b

-- Constraint synonym for all operations on vectors.
-- type VECTOR s ty a = (Storable a, IsVector ty, SingI ty)

-- | Constraint synonym for all operations on vectors.
type SVECTOR ty a = (Storable a, IsVector ty, SingI ty, ElemRep V ty ~ a)
