-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GADTs #-}

module H.HExp (HExp(..){-, view-}) where

import H.Constraints
import qualified Foreign.R.Type as R
import qualified Foreign.R as R
import           Foreign.R (SEXP, SEXPTYPE)

import Control.Applicative
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BU
import Data.Complex (Complex)
import Foreign (Ptr, newForeignPtr_ )

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
