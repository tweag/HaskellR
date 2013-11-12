-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GADTs #-}

module H.HExp (HExp(..){-, view-}) where

import H.Constraints
import qualified Foreign.R.Type as R
import           Foreign.R (SEXP, SEXPTYPE)

import Data.Int (Int32)
import Data.Word (Word8)
import Data.Vector.Storable (Vector)
import Data.ByteString (ByteString)
import Data.Complex (Complex)
import Foreign (Ptr)

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
  Lang      :: (a :∈ R.Symbol :+: R.Lang)         -- XXX R.Closure also?
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
  Int       :: {-# UNPACK #-} !(Vector Int32)
            -> HExp (R.Vector Int32)
  Real      :: {-# UNPACK #-} !(Vector Double)
            -> HExp (R.Vector Int32)
  Complex   :: {-# UNPACK #-} !(Vector (Complex Double))
            -> HExp (R.Vector (Complex Double))
  String    :: {-# UNPACK #-} !(Vector (SEXP (R.Vector Word8)))
            -> HExp (R.Vector (SEXP (R.Vector Word8)))
  -- Fields: pairlist of promises.
  DotDotDot :: SEXP R.PairList
            -> HExp R.List
  Any       :: HExp a
  -- Fields: truelength, content.
  Vector    :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector (SEXP R.Any))
            -> HExp (R.Vector (SEXP R.Any))
  -- Fields: truelength, content.
  Expr      :: {-# UNPACK #-} !Int32
            -> !(Vector (SEXP R.Any))
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

{-
-- | Converts R SEXP into the Haskell view no changes with data are made so
-- R GC is capable for data collection and such types are not safe to use
-- inside Haskell environment without additional actions.
--
-- Complexity: O(depth(S))
view :: R.SEXP a -> IO (HExp b)
view s = do
    ty <- R.typeOf s
    case ty of
      R.Nil ->
        return Nil
      R.Symbol ->
        Symbol <$> (R.printName s)
               <*> (R.symValue s)
               <*> (R.symInternal s)
      R.List ->
        List <$> R.car s
             <*> R.cdr s
             <*> R.tag s
--      R.Int  -> Int `vector` (mkVec R.integer)
--      R.Real -> Real`vector` (mkVec R.real)
{-
      R.Env  ->
        Env  <$> R.frame s
             <*> R.enclos s
             <*> R.hashtab s
-}
{-
    case ty of
      R.EnvSXP ->
      R.CloSXP ->
        Closure
          <$> (view =<< R.formals s)
          <*> (view =<< R.body s)
          <*> (view =<< R.cloEnv s)
      R.PromSXP ->
        Promise
          <$> (view =<< R.prValue s)
          <*> (view =<< R.prCode s)
          <*> (view =<< R.prEnv s)
      R.LangSXP ->
        Lang
          <$> (view =<< R.car s)
          <*> (view =<< R.cdr s)
      R.SpecialSXP -> return $ Special 0 -- XXX: read correct offset
      R.BuiltinSXP -> return $ Builtin 0 -- XXX: read correct offset
      R.CplxSXP ->
        unimplemented "convertHEXP" ty
        -- HExp.Complex `vector` (mkVec R.complex)
      R.StrSXP  -> String `vector` (mkVec R.string)
      R.VecSXP  -> Vector `vector` (mkVec R.vector)
      R.ExpSXP  -> Expr `vector` (mkVec R.expression)
      R.CharSXP    ->
        Char `vector`
          (\d l -> BU.unsafePackCStringLen . (flip (,) l) =<< R.char d)
      R.RawSXP  ->
        Raw `vector`
          (\d l -> BU.unsafePackCStringLen . (flip (,) l) =<< R.raw d)
      R.AnySXP  -> return Any
      R.DotSXP  ->
        unimplemented "convertHEXP" ty
        -- HExp.DotDotDot <$> R.args s
      R.BCodeSXP ->
        -- XXX: doesn't know what to do with it
        unimplemented "converthexp" ty
      R.ExtptrSXP  ->
        -- XXX: require low level access
        unimplemented "converthexp" ty
      {-
        HExp.ExtPtr
          <$> (convertHEXP <$> R.pointer s)
          <*> (convertHEXP <$> R.protectionValue s)
          <*> (convertHEXP <$> R.tag)
          -}
      R.WeakRefSXP ->
        --- XXX: require low level access
        unimplemented "convertHEXP" ty
{-
        HExp.WeakRef
          <$> (fromIntegral <$> R.length s)
          <*> (fromIntegral <$> R.trueLength s)
          <*> (convertHEXP =<< R.key s)
          <*> (convertHEXP =<< R.value s)
          <*> (convertHEXP =<< R.finalizer s)
          <*> (convertHEXP =<< R.next s)
      R.S4XSXP -> S4 <$> (view =<< R.tag s)
-}
  where
    mkVec :: (V.Storable a) => (R.SEXP -> IO (Ptr a))  -> (R.SEXP -> Int -> IO (V.Vector a))
    mkVec f = \s l -> flip V.unsafeFromForeignPtr0 l <$> (newForeignPtr_ =<< f s)
    vector :: (Int32 -> Int32 -> a -> b) -> (R.SEXP -> Int -> IO a) -> IO b
    vector f g = do
        l <- R.length s
        f (fromIntegral l)
          <$> (fromIntegral <$> R.trueLength s)
          <*> g s l
    unimplemented :: Show a => String -> a -> b
    unimplemented f a = error $ f ++ ": unimplemented " ++ show a
-}
-}
