-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GADTs #-}

module H.HExp (HExp(..)) where

import H.Constraints
import qualified Foreign.R.Type as R
import qualified Foreign.R as R
import           Foreign.R (SEXP, SEXPTYPE)

import Data.Int (Int32)
import Data.Word (Word8)
import Data.Vector.Storable (Vector)
import Data.ByteString (ByteString)
import Data.Complex (Complex)
import Foreign (Ptr)

-- Use explicit UNPACK pragmas rather than -funbox-strict-fields in order to get
-- warnings if a field is not unpacked when we expect it to.

data HExp :: SEXPTYPE -> * where
  -- Primitive types. The field names match those of <RInternals.h>.
  Nil       :: HExp a
  Symbol    :: SEXP (R.Vector Word8)              -- ^ pname
            -> SEXP a                             -- ^ value
            -> SEXP b                             -- ^ internal
            -> HExp R.Symbol
  List      :: SEXP a                             -- ^ carval
            -> SEXP R.List                        -- ^ cdrval
            -> SEXP R.Symbol                      -- ^ tagval
            -> HExp R.List
  Env       :: SEXP R.PairList                    -- ^ frame
            -> SEXP R.Env                         -- ^ enclos
            -> SEXP (R.Vector R.PairList)         -- ^ hashtab
            -> HExp R.Env
  Closure   :: SEXP R.PairList                    -- ^ formals
            -> SEXP a                             -- ^ body
            -> SEXP R.Env                         -- ^ env
            -> HExp R.Closure
  Promise   :: SEXP a                             -- ^ value
            -> SEXP b                             -- ^ expr
            -> SEXP R.Env                         -- ^ env
            -> HExp R.Promise
  -- Derived types. These types don't have their own 'struct' declaration in
  -- <Rinternals.h>.
  Lang      :: (a :∈ R.Symbol :+: R.Lang)         -- XXX R.Closure also?
            => SEXP a                             -- ^ function
            -> SEXP R.List                        -- ^ args
            -> HExp R.List
  Special   :: {-# UNPACK #-} !Int32              -- ^ offset
            -> HExp R.Special
  Builtin   :: {-# UNPACK #-} !Int32              -- ^ offset
            -> HExp R.Builtin
  Char      :: {-# UNPACK #-} !ByteString
            -> HExp (R.Vector Word8)
  Int       :: {-# UNPACK #-} !(Vector Int32)
            -> HExp (R.Vector Int32)
  Real      :: {-# UNPACK #-} !(Vector Double)
            -> HExp (R.Vector Int32)
  Complex   :: {-# UNPACK #-} !(Vector (Complex Double))
            -> HExp (R.Vector (Complex Double))
  String    :: {-# UNPACK #-} !(Vector (SEXP (R.Vector Word8)))
            -> HExp (R.Vector (SEXP (R.Vector Word8)))
  DotDotDot :: SEXP R.List                        -- ^ pairlist of promises
            -> HExp R.List
  Any       :: HExp a
  Vector    :: {-# UNPACK #-} !Int32              -- ^ truelength
            -> {-# UNPACK #-} !(Vector (SEXP R.Any))
            -> HExp (R.Vector (SEXP R.Any))
  Expr      :: {-# UNPACK #-} !Int32              -- ^ truelength
            -> !(Vector (SEXP R.Any))
            -> HExp (R.Vector (SEXP R.Any))
  Bytecode  :: HExp a -- XXX
  ExtPtr    :: Ptr a                              -- ^ pointer
            -> SEXP b                             -- ^ protectionValue
            -> SEXP R.Symbol                      -- ^ tagval
            -> HExp R.ExtPtr
  WeakRef   :: (a :∈ R.Nil :+: R.Env :+: R.ExtPtr)
            => SEXP a                             -- ^ key
            -> SEXP b                             -- ^ value
            -> SEXP c                             -- ^ finalizer
            -> SEXP d                             -- ^ next
            -> HExp R.WeakRef
  Raw       :: {-# UNPACK #-} !ByteString
            -> HExp R.Raw
  S4        :: SEXP R.Symbol                      -- ^ tagval
            -> HExp R.S4
