-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-@ LIQUID "--exact-data-cons" @-}

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
  ElemRep s 'SChar    = Word8
  ElemRep s 'Logical = Logical
  ElemRep s 'SInt     = Int32
  ElemRep s 'Real    = Double
  ElemRep s 'SComplex = Complex Double
  ElemRep s 'SString  = SEXP s -- SEXP s 'Char
  ElemRep s 'SVector  = SEXP s
  ElemRep s 'Expr    = SEXP s
  ElemRep s 'Raw     = Word8

data VSEXPTYPE s a where
  VChar :: VSEXPTYPE s Word8
  VLogical :: VSEXPTYPE s Logical
  VInt :: VSEXPTYPE s Int32
  VReal :: VSEXPTYPE s Double
  VComplex :: VSEXPTYPE s (Complex Double)
  VString :: VSEXPTYPE s (SEXP s)
  VVector :: VSEXPTYPE s (SEXP s)
  VExpr :: VSEXPTYPE s (SEXP s)
  VRaw :: VSEXPTYPE s Word8


{-@ reflect vstypeOf @-}
vstypeOf :: VSEXPTYPE s a -> SEXPTYPE
vstypeOf VChar = SChar
vstypeOf VLogical = Logical
vstypeOf VInt = SInt
vstypeOf VReal = Real
vstypeOf VComplex = SComplex
vstypeOf VString = SString
vstypeOf VVector = SVector
vstypeOf VExpr = Expr
vstypeOf VRaw = Raw

-- | 'ElemRep' in the form of a relation, for convenience.
type E s a b = ElemRep s a ~ b

-- Constraint synonym for all operations on vectors.
-- type VECTOR s ty a = (Storable a, IsVector ty, SingI ty)

-- | Constraint synonym for all operations on vectors.
-- type SVECTOR ty a = (Storable a, IsVector ty, SingI ty, ElemRep V ty ~ a)
