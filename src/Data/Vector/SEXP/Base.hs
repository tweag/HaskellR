-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.SEXP.Base where

import Foreign.R.Type
import Foreign.R (SEXP, SomeSEXP)

import Data.Singletons (SingI)

import Data.Complex (Complex)
import Data.Word (Word8)
import Data.Int (Int32)
import Foreign.Storable (Storable)

-- | Function from R types to the types of the representations of each element
-- in the vector.
type family ElemRep s (a :: SEXPTYPE)
type instance ElemRep s 'Char    = Word8
type instance ElemRep s 'Logical = Logical
type instance ElemRep s 'Int     = Int32
type instance ElemRep s 'Real    = Double
type instance ElemRep s 'Complex = Complex Double
type instance ElemRep s 'String  = SEXP s 'Char
type instance ElemRep s 'Vector  = SomeSEXP s
type instance ElemRep s 'Expr    = SomeSEXP s
type instance ElemRep s 'Raw     = Word8

-- | 'ElemRep' in the form of a relation, for convenience.
type E s a b = ElemRep s a ~ b

-- | Constraint synonym for all operations on vectors.
type VECTOR s ty a = (Storable a, IsVector ty, SingI ty, ElemRep s ty ~ a)
