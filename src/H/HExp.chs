-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.HExp
  ( HExp(..)
  , hexp
  ) where

import H.Constraints
import qualified Foreign.R      as R
import qualified Foreign.R.Type as R
import           Foreign.R (SEXP, SEXPREC, SEXPTYPE)

import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString.Unsafe as BS
import           Data.ByteString (ByteString)

import Control.Applicative
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import GHC.Ptr (Ptr(..), plusPtr)
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

{# import qualified Foreign.R #} ()
#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
{#pointer *SEXPREC as SEXP0 -> SEXPREC #}

-- Use explicit UNPACK pragmas rather than -funbox-strict-fields in order to get
-- warnings if a field is not unpacked when we expect it to.

-- | A view of R's internal 'SEXP' structure as an algebraic datatype. Because
-- this is in fact a GADT, the use of named record fields is not possible here.
-- Named record fields give rise to functions for whom it is not possible to
-- assign a reasonable type (existentially quantified type variables would
-- escape).
--
-- Note further that Haddock does not currently support constructor comments
-- when using the GADT syntax.
data HExp :: SEXPTYPE -> * where
  -- Primitive types. The field names match those of <RInternals.h>.
  Nil       :: HExp a
  -- Fields: pname, value, internal.
  Symbol    :: SEXP (R.Vector Word8)
            -> SEXP a
            -> SEXP b
            -> HExp R.Symbol
  -- Fields: carval, cdrval, tagval.
  List      :: SEXP a
            -> SEXP R.List
            -> SEXP R.Symbol
            -> HExp R.List
  -- Fields: frame, enclos, hashtab.
  Env       :: SEXP R.PairList
            -> SEXP R.Env
            -> SEXP (R.Vector R.PairList)
            -> HExp R.Env
  -- Fields: formals, body, env.
  Closure   :: SEXP R.PairList
            -> SEXP a
            -> SEXP R.Env
            -> HExp R.Closure
  -- Fields: value, expr, env.
  Promise   :: SEXP a
            -> SEXP b
            -> SEXP R.Env
            -> HExp R.Promise
  -- Derived types. These types don't have their own 'struct' declaration in
  -- <Rinternals.h>.
  -- Fields: function, args.
  Lang      :: () -- (a :∈ R.Symbol :+: R.Lang)         -- XXX R.Closure also?
            => SEXP a
            -> SEXP R.List
            -> HExp R.List
  -- Fields: offset.
  Special   :: {-# UNPACK #-} !Int32
            -> HExp R.Special
  -- Fields: offset.
  Builtin   :: {-# UNPACK #-} !Int32
            -> HExp R.Builtin
  Char      :: {-# UNPACK #-} !ByteString
            -> HExp (R.Vector Word8)
  Int       :: {-# UNPACK #-} !(Vector.Vector Int32)
            -> HExp (R.Vector Int32)
  Real      :: {-# UNPACK #-} !(Vector.Vector Double)
            -> HExp (R.Vector Int32)
  Complex   :: {-# UNPACK #-} !(Vector.Vector (Complex Double))
            -> HExp (R.Vector (Complex Double))
  String    :: {-# UNPACK #-} !(Vector.Vector (SEXP (R.Vector Word8)))
            -> HExp (R.Vector (SEXP (R.Vector Word8)))
  -- Fields: pairlist of promises.
  DotDotDot :: SEXP R.PairList
            -> HExp R.List
  Any       :: HExp a
  -- Fields: truelength, content.
  Vector    :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.Vector (SEXP R.Any))
            -> HExp (R.Vector (SEXP R.Any))
  -- Fields: truelength, content.
  Expr      :: {-# UNPACK #-} !Int32
            -> !(Vector.Vector (SEXP R.Any))
            -> HExp (R.Vector (SEXP R.Any))
  Bytecode  :: HExp a -- XXX
  -- Fields: pointer, protectionValue, tagval
  ExtPtr    :: Ptr a
            -> SEXP b
            -> SEXP R.Symbol
            -> HExp R.ExtPtr
  -- Fields: key, value, finalizer, next.
  WeakRef   :: (a :∈ R.Nil :+: R.Env :+: R.ExtPtr)
            => SEXP a
            -> SEXP b
            -> SEXP c
            -> SEXP d
            -> HExp R.WeakRef
  Raw       :: {-# UNPACK #-} !ByteString
            -> HExp R.Raw
  -- Fields: tagval.
  S4        :: SEXP R.Symbol
            -> HExp R.S4

-- XXX Orphan instance. Could find a better place to put it.
instance (Fractional a, Real a, Storable a) => Storable (Complex a) where
  sizeOf _ = {#sizeof Rcomplex #}
  alignment _ = {#alignof Rcomplex #}
  poke cptr (r :+ i) = do
      {#set Rcomplex->r #} cptr (realToFrac r)
      {#set Rcomplex->i #} cptr (realToFrac i)
  peek cptr =
      (:+) <$> (realToFrac <$> {#get Rcomplex->r #} cptr)
           <*> (realToFrac <$> {#get Rcomplex->i #} cptr)

instance Storable (HExp a) where
  sizeOf _ = {#sizeof SEXPREC #}
  alignment _ = {#alignof SEXPREC #}
  poke = error "Unimplemented."
  peek = peekHExp

peekHExp :: SEXP a -> IO (HExp a)
peekHExp s = do
    let coerce :: IO (HExp a) -> IO (HExp b)
        coerce = unsafeCoerce

        vector :: forall a. Storable a => IO (Vector.Vector a)
        vector = do
          len <- {#get VECSEXP->vecsxp.length #} s
          fptr <- castForeignPtr <$> newForeignPtr_ s :: IO (ForeignPtr a)
          return $ Vector.unsafeFromForeignPtr
                     fptr
                     {#sizeof SEXPREC_ALIGN #}
                     (fromIntegral len)

        bytestring :: IO ByteString
        bytestring = do
          len <- {#get VECSEXP->vecsxp.length #} s
          let !(Ptr addr#) = s `plusPtr` {#sizeof SEXPREC_ALIGN #}
          BS.unsafePackAddressLen (fromIntegral len) addr#

    R.typeOf s >>= \case
      R.Nil       -> coerce $ return Nil
      R.Symbol    -> coerce $
        Symbol    <$> (R.sexp <$> {#get SEXP->u.symsxp.pname #} s)
                  <*> (R.sexp <$> {#get SEXP->u.symsxp.value #} s)
                  <*> (R.sexp <$> {#get SEXP->u.symsxp.internal #} s)
      R.List      -> coerce $
        List      <$> (R.sexp <$> {#get SEXP->u.listsxp.carval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.tagval #} s)
      R.Env       -> coerce $
        Env       <$> (R.sexp <$> {#get SEXP->u.envsxp.frame #} s)
                  <*> (R.sexp <$> {#get SEXP->u.envsxp.enclos #} s)
                  <*> (R.sexp <$> {#get SEXP->u.envsxp.hashtab #} s)
      R.Closure   -> coerce $
        Closure   <$> (R.sexp <$> {#get SEXP->u.closxp.formals #} s)
                  <*> (R.sexp <$> {#get SEXP->u.closxp.body #} s)
                  <*> (R.sexp <$> {#get SEXP->u.closxp.env #} s)
      R.Promise   -> coerce $
        Promise   <$> (R.sexp <$> {#get SEXP->u.promsxp.value #} s)
                  <*> (R.sexp <$> {#get SEXP->u.promsxp.expr #} s)
                  <*> (R.sexp <$> {#get SEXP->u.promsxp.env #} s)
      R.Lang      -> coerce $
        Lang      <$> (R.sexp <$> {#get SEXP->u.listsxp.carval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s)
      R.Special   -> coerce $
        Special   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} s)
      R.Builtin   -> coerce $
        Builtin   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} s)
      R.Char      -> coerce $ Char    <$> bytestring
      R.Int       -> coerce $ Int     <$> vector
      R.Real      -> coerce $ Real    <$> vector
      R.Complex   -> coerce $ Complex <$> vector
      R.String    -> coerce $ error "Unimplemented."
      R.DotDotDot -> coerce $ error "Unimplemented."
      R.Any       -> return Any
      R.Vector _  -> coerce $ error "Unimplemented."
      R.Expr      -> coerce $ error "Unimplemented."
      R.Bytecode  -> coerce $ error "Unimplemented."
      R.ExtPtr    -> coerce $ error "Unimplemented."
      R.WeakRef   -> coerce $ error "Unimplemented."
      R.Raw       -> coerce $ error "Unimplemented."
      R.S4        -> coerce $ error "Unimplemented."
      _           -> coerce $ error "Unimplemented."

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
hexp :: SEXP a -> HExp a
hexp = unsafePerformIO . peek
