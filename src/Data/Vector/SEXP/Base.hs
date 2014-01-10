-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.SEXP.Base where

import Foreign.R.Type
import Foreign.R (SEXP, SomeSEXP)

import Data.Complex (Complex)
import Data.Word (Word8)
import Data.Int (Int32)

-- | Function from R types to the types of the representations of each element
-- in the vector.
type family ElemRep (a :: SEXPTYPE)
type instance ElemRep 'Char    = Word8
type instance ElemRep 'Logical = Logical
type instance ElemRep 'Int     = Int32
type instance ElemRep 'Real    = Double
type instance ElemRep 'Complex = Complex Double
type instance ElemRep 'String  = SEXP 'Char
type instance ElemRep 'Vector  = SomeSEXP
type instance ElemRep 'Expr    = SomeSEXP
type instance ElemRep 'Raw     = Word8

-- | 'ElemRep' in the form of a relation, for convenience.
type E a b = ElemRep a ~ b
