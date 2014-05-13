-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Provides a /shallow/ view of a 'SEXP' R value as an algebraic datatype. This
-- is useful to define functions over R values in Haskell with pattern matching.
-- For example:
--
-- @
-- toPair :: SEXP a -> (SomeSEXP, SomeSEXP)
-- toPair (hexp -> List _ (Just car) (Just cdr)) = (SomeSEXP car, SomeSEXP cdr)
-- toPair (hexp -> Lang car (Just cdr)) = (SomeSEXP car, SomeSEXP cdr)
-- toPair s = error $ "Cannot extract pair from object of type " ++ typeOf s
-- @
--
-- (See 'Foreign.R.SomeSEXP' for why we need to use it here.)
--
-- The view is said to be 'shallow' because it only unfolds the head of the
-- R value into an algebraic datatype. In this way, functions producing views
-- can be written non-recursively, hence inlined at all call sites and
-- simplified away. When produced by a view function in a pattern match,
-- allocation of the view can be compiled away and hence producing a view can be
-- done at no runtime cost. In fact, pattern matching on a view in this way is
-- more efficient than using the accessor functions defined in "Foreign.R",
-- because we avoid the overhead of calling one or more FFI functions entirely.
--
-- 'HExp' is the /view/ and 'hexp' is the /view function/ that projects 'SEXP's
-- into 'HExp' views.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.HExp
  ( HExp(..)
  , hexp
  , unhexp
  , unhexpIO
  , vector
  , selfSymbol
  ) where

import H.Internal.Prelude
import qualified Language.R.Globals as H
import qualified Foreign.R      as R
import qualified Foreign.R.Type as R
import           Foreign.R (SEXPREC)
import           Language.R.GC (withProtected)

import qualified Data.Vector.SEXP as Vector

import Control.Applicative
import Control.Monad ( void )
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Complex
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign.C
import Foreign ( castPtr, nullPtr )
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

#define USE_RINTERNALS
#include "Hcompat.h"
#include <R.h>
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
  Nil       :: HExp R.Nil
  -- Fields: pname, value, internal.
  Symbol    :: SEXP R.Char
            -> SEXP a
            -> Maybe (SEXP b)
            -> HExp R.Symbol
  -- Fields: carval, cdrval, tagval.
  List      :: SEXP a
            -> !(Maybe (SEXP R.List))
            -> !(Maybe (SEXP R.Symbol))
            -> HExp R.List
  -- Fields: frame, enclos, hashtab.
  Env       :: SEXP R.PairList
            -> SEXP R.Env
            -> SEXP R.Vector
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
            -> Maybe (SEXP R.List)
            -> HExp R.Lang
  -- Fields: offset.
  Special   :: {-# UNPACK #-} !Int32
            -> HExp R.Special
  -- Fields: offset.
  Builtin   :: {-# UNPACK #-} !Int32
            -> HExp R.Builtin
  Char      :: {-# UNPACK #-} !(Vector.Vector R.Char Word8)
            -> HExp R.Char
  Logical   :: {-# UNPACK #-} !(Vector.Vector 'R.Logical R.Logical)
            -> HExp 'R.Logical
  Int       :: {-# UNPACK #-} !(Vector.Vector R.Int Int32)
            -> HExp R.Int
  Real      :: {-# UNPACK #-} !(Vector.Vector R.Real Double)
            -> HExp R.Real
  Complex   :: {-# UNPACK #-} !(Vector.Vector R.Complex (Complex Double))
            -> HExp R.Complex
  String    :: {-# UNPACK #-} !(Vector.Vector R.String (SEXP R.Char))
            -> HExp R.String
  -- Fields: pairlist of promises.
  DotDotDot :: SEXP R.PairList
            -> HExp R.List
  -- Fields: truelength, content.
  Vector    :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.Vector R.Vector SomeSEXP)
            -> HExp R.Vector
  -- Fields: truelength, content.
  Expr      :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.Vector R.Expr SomeSEXP)
            -> HExp R.Expr
  Bytecode  :: HExp a -- XXX
  -- Fields: pointer, protectionValue, tagval
  ExtPtr    :: Ptr ()
            -> SEXP b
            -> SEXP R.Symbol
            -> HExp R.ExtPtr
  -- Fields: key, value, finalizer, next.
  WeakRef   :: -- (a :∈ R.Nil :+: R.Env :+: R.ExtPtr
               -- ,c :∈ R.Nil :+: R.Closure :+: R.Builtin :+: R.Special)
               SEXP a
            -> SEXP b
            -> SEXP c
            -> SEXP d
            -> HExp R.WeakRef
  Raw       :: {-# UNPACK #-} !(Vector.Vector R.Raw Word8)
            -> HExp R.Raw
  -- Fields: tagval.
  S4        :: SEXP a
            -> HExp R.S4

-- | Wrapper for partially applying a type synonym.
newtype E a = E (SEXP a)

instance HEq E where
  E (hexp -> t1) === E (hexp -> t2) = t1 === t2

heqMaybe :: Maybe (SEXP a) -> Maybe (SEXP b) -> Bool
heqMaybe (Just x) (Just y) = E x === E y
heqMaybe Nothing Nothing = True
heqMaybe _ _  = False

instance HEq HExp where
  Nil === Nil = True
  Symbol pname1 value1 internal1 === Symbol pname2 value2 internal2 =
      E pname1 === E pname2 &&
      E value1 === E value2 &&
      (fromJust $ (===) <$> fmap E internal1 <*> fmap E internal2 <|> return True)
  List carval1 cdrval1 tagval1 === List carval2 cdrval2 tagval2 =
      E carval1 === E carval2 &&
      (fromJust $ (===) <$> fmap E cdrval1 <*> fmap E cdrval2 <|> return True) &&
      (fromJust $ (===) <$> fmap E tagval1 <*> fmap E tagval2 <|> return True)
  Env frame1 enclos1 hashtab1 === Env frame2 enclos2 hashtab2 =
      E frame1 === E frame2 &&
      E enclos1 === E enclos2 &&
      E hashtab1 === E hashtab2
  Closure formals1 body1 env1 === Closure formals2 body2 env2 =
      E formals1 === E formals2 &&
      E body1 === E body2 &&
      E env1 === E env2
  Promise value1 expr1 env1 === Promise value2 expr2 env2 =
      E value1 === E value2 &&
      E expr1 === E expr2 &&
      E env1 === E env2
  Lang carval1 cdrval1 === Lang carval2 cdrval2 =
      E carval1 === E carval2 &&
      (cdrval1 `heqMaybe` cdrval2)
  Special offset1 === Special offset2 =
      offset1 ==  offset2
  Builtin offset1 === Builtin offset2 =
      offset1 == offset2
  Char vec1 === Char vec2 =
      vec1 == vec2
  Int vec1 === Int vec2 =
      vec1 == vec2
  Real vec1 === Real vec2 =
      vec1 == vec2
  String vec1 === String vec2 =
      vec1 == vec2
  Complex vec1 === Complex vec2 =
      vec1 == vec2
  DotDotDot pairlist1 === DotDotDot pairlist2 =
      E pairlist1 === E pairlist2
  Vector truelength1 vec1 === Vector truelength2 vec2 =
      let eq (SomeSEXP s1) (SomeSEXP s2) = E s1 === E s2
      in truelength1 == truelength2 &&
         and (zipWith eq  (Vector.toList vec1) (Vector.toList vec2))
  Expr truelength1 vec1 === Expr truelength2 vec2 =
      let eq (SomeSEXP s1) (SomeSEXP s2) = E s1 === E s2
      in truelength1 == truelength2 &&
         and (zipWith eq (Vector.toList vec1) (Vector.toList vec2))
  Bytecode === Bytecode = True
  ExtPtr pointer1 protectionValue1 tagval1 === ExtPtr pointer2 protectionValue2 tagval2 =
      castPtr pointer1 == castPtr pointer2 &&
      E protectionValue1 === E protectionValue2 &&
      E tagval1 === E tagval2
  WeakRef key1 value1 finalizer1 next1 === WeakRef key2 value2 finalizer2 next2 =
      E key1 === E key2 &&
      E value1 === E value2 &&
      E finalizer1 === E finalizer2 &&
      E next1 === E next2
  Raw vec1 === Raw vec2 =
      vec1 == vec2
  S4 tagval1 === S4 tagval2 =
      E tagval1 === E tagval2
  _ === _ = False

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
  poke = pokeHExp
  peek = peekHExp
  {-# INLINE peek #-}

{-# INLINE peekHExp #-}
peekHExp :: SEXP a -> IO (HExp a)
peekHExp s = do
    let coerce :: IO (HExp a) -> IO (HExp b)
        coerce = unsafeCoerce

    case R.typeOf s of
      R.Nil       -> coerce $ return Nil
      R.Symbol    -> coerce $
        Symbol    <$> (R.sexp <$> {#get SEXP->u.symsxp.pname #} s)
                  <*> (R.sexp <$> {#get SEXP->u.symsxp.value #} s)
                  <*> (maybeNil =<< (R.sexp <$> {#get SEXP->u.symsxp.internal #} s))
      R.List      -> coerce $
        List      <$> (R.sexp <$> {#get SEXP->u.listsxp.carval #} s)
                  <*> (maybeNil =<< (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s))
                  <*> (maybeNil =<< (R.sexp <$> {#get SEXP->u.listsxp.tagval #} s))
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
                  <*> (maybeNil =<< (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s))
      R.Special   -> coerce $
        Special   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} s)
      R.Builtin   -> coerce $
        Builtin   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} s)
      R.Char      -> coerce $ Char    <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Logical   -> coerce $ Logical <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Int       -> coerce $ Int     <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Real      -> coerce $ Real    <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Complex   -> coerce $ Complex <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.String    -> coerce $ String  <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.DotDotDot -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)
      R.Vector    -> coerce $
        Vector    <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} s)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Expr      -> coerce $
        Expr      <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} s)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Bytecode  -> return $ Bytecode
      R.ExtPtr    -> coerce $
        ExtPtr    <$> (castPtr <$> {#get SEXP->u.listsxp.carval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.tagval #} s)
      R.WeakRef   -> coerce $
        WeakRef   <$> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 0)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 1)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 2)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 3)
      R.Raw       -> coerce $ Raw     <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.S4        -> coerce $ 
        S4        <$> (R.sexp <$> {# get SEXP->u.listsxp.tagval #} s)
      _           -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)

pokeHExp :: Ptr (HExp a) -> HExp a -> IO ()
pokeHExp s h = do
    case h of
         Nil -> return ()
         Symbol pname value internal -> do
           {#set SEXP->u.symsxp.pname #} s (R.unsexp pname)
           {#set SEXP->u.symsxp.value #} s (R.unsexp value)
           maybe ({#set SEXP->u.symsxp.internal#} s (R.unsexp H.nilValue))
                 ({#set SEXP->u.symsxp.internal#} s . R.unsexp) internal
         List carval cdrval tagval -> do
           {#set SEXP->u.listsxp.carval #} s (R.unsexp carval)
           maybe ({#set SEXP->u.listsxp.cdrval#} s (R.unsexp H.nilValue))
                 ({#set SEXP->u.listsxp.cdrval#} s . R.unsexp) cdrval
           maybe ({#set SEXP->u.listsxp.tagval#} s (R.unsexp H.nilValue))
                 ({#set SEXP->u.listsxp.tagval#} s . R.unsexp) tagval
         Env frame enclos hashtab -> do
           {#set SEXP->u.envsxp.frame #} s (R.unsexp frame)
           {#set SEXP->u.envsxp.enclos #} s (R.unsexp enclos)
           {#set SEXP->u.envsxp.hashtab #} s (R.unsexp hashtab)
         Closure formals body env -> do
           {#set SEXP->u.closxp.formals #} s (R.unsexp formals)
           {#set SEXP->u.closxp.body #} s (R.unsexp body)
           {#set SEXP->u.closxp.env #} s (R.unsexp env)
         Promise value expr env -> do
           {#set SEXP->u.promsxp.value #} s (R.unsexp value)
           {#set SEXP->u.promsxp.expr #} s (R.unsexp expr)
           {#set SEXP->u.promsxp.env #} s (R.unsexp env)
         Lang carval cdrval -> do
           {#set SEXP->u.listsxp.carval #} s (R.unsexp carval)
           maybe ({#set SEXP->u.listsxp.cdrval#} s (R.unsexp H.nilValue))
                 ({#set SEXP->u.listsxp.cdrval #} s . R.unsexp) cdrval
         Special offset -> do
           {#set SEXP->u.primsxp.offset #} s (fromIntegral offset)
         Builtin offset -> do
           {#set SEXP->u.primsxp.offset #} s (fromIntegral offset)
         Char _vc        -> unimplemented "pokeHExp"
         Logical  _vt    -> unimplemented "pokeHExp"
         Int  _vt        -> unimplemented "pokeHExp"
         Real _vt        -> unimplemented "pokeHExp"
         String _vt      -> unimplemented "pokeHExp"
         Complex _vt     -> unimplemented "pokeHExp"
         Vector _v _     -> unimplemented "pokeHExp"
         Bytecode        -> unimplemented "pokeHExp"
         ExtPtr _ _ _    -> unimplemented "pokeHExp"
         WeakRef _ _ _ _ -> unimplemented "pokeHExp"
         Raw     _       -> unimplemented "pokeHExp"
         S4      _       -> unimplemented "pokeHExp"
         DotDotDot _     -> unimplemented "pokeHExp"
         Expr _ _        -> unimplemented "pokeHExp"

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
hexp :: SEXP a -> HExp a
hexp = unsafeInlineIO . peek
{-# INLINE hexp #-}

-- | Inverse hexp view to the real structure, note that for scalar types
-- hexp will allocate new SEXP, and @unhexp . hexp@ is not an identity function.
-- however for vector types it will return original SEXP.
unhexp :: HExp a -> SEXP a
unhexp = unsafePerformIO . unhexpIO
{-# NOINLINE unhexp #-}

-- The basic idea over unhexpIO is that fields of non vector elements are lazy
-- so they will be forced in poke (so inside withProtected object) this guarantees
-- the safeness of memory allocation.
unhexpIO :: HExp a -> IO (SEXP a)
unhexpIO   Nil         = return H.nilValue
unhexpIO s@(Symbol{})  =
    withProtected (R.allocSEXP R.SSymbol) (\x -> poke x s >> return x)
unhexpIO (List c md mt) = do
    void $ R.protect c
    void $ R.protect d
    void $ R.protect t
    z <- R.cons c d
    {# set SEXP->u.listsxp.tagval#} z (R.unsexp t)
    R.unprotect 3 -- - $ maybe 2 (const 3) mt
    return z
  where
    d = maybe (R.unsafeCoerce H.nilValue) id md
    t = maybe (R.unsafeCoerce H.nilValue) id mt
unhexpIO (Lang carval mbcdrval)   = do
    let cdrval = maybe (R.unsafeCoerce H.nilValue) id mbcdrval
    void $ R.protect carval
    void $ R.protect cdrval
    x <- R.allocSEXP R.SLang
    R.setCar x carval
    R.setCdr x cdrval
    R.unprotect 2
    return x
unhexpIO s@(Env{})     =
    withProtected (R.allocSEXP R.SEnv) (\x -> poke x s >> return x)
unhexpIO s@(Closure{}) =
    withProtected (R.allocSEXP R.SClosure) (\x -> poke x s >> return x)
unhexpIO s@(Special{}) =
    withProtected (R.allocSEXP R.SSpecial) (\x -> poke x s >> return x)
unhexpIO s@(Builtin{}) =
    withProtected (R.allocSEXP R.SBuiltin) (\x -> poke x s >> return x)
unhexpIO s@(Promise{}) =
    withProtected (R.allocSEXP R.SPromise) (\x -> poke x s >> return x)
unhexpIO  (Bytecode{}) = unimplemented "unhexp"
unhexpIO (Real vt)     = Vector.unsafeToSEXP vt
unhexpIO (Logical vt)  = Vector.unsafeToSEXP vt
unhexpIO (Int vt)      = Vector.unsafeToSEXP vt
unhexpIO (Complex vt)  = Vector.unsafeToSEXP vt
unhexpIO (Vector _ vt) = Vector.unsafeToSEXP vt
unhexpIO (Char vt)     = Vector.unsafeToSEXP vt
unhexpIO (String vt)   = Vector.unsafeToSEXP vt
unhexpIO (Raw vt)      = Vector.unsafeToSEXP vt
unhexpIO S4{}          = unimplemented "unhexp"
unhexpIO (Expr _ vt)   = Vector.unsafeToSEXP vt
unhexpIO WeakRef{}     = error "unhexp does not support WeakRef, use Foreign.R.mkWeakRef instead."
unhexpIO DotDotDot{}   = unimplemented "unhexp"
unhexpIO ExtPtr{}      = unimplemented "unhexp"

-- | Project the vector out of 'SEXP's.
vector :: R.IsVector a => SEXP a -> Vector.Vector a (Vector.ElemRep a)
vector (hexp -> Char vec)     = vec
vector (hexp -> Logical vec)  = vec
vector (hexp -> Int vec)      = vec
vector (hexp -> Real vec)     = vec
vector (hexp -> Complex vec)  = vec
vector (hexp -> String vec)   = vec
vector (hexp -> Vector _ vec) = vec
vector (hexp -> Expr _ vec)   = vec
vector s = violation "vector" $ show (R.typeOf s) ++ " unexpected vector type."

-- | Check if SEXP is nill
maybeNil :: SEXP a
         -> IO (Maybe (SEXP a))
maybeNil s = do
  return $
    if R.unsexp s == R.unsexp H.nilValue
      then Nothing
      else Just s

-- | Symbols can have values attached to them. This function creates a symbol
-- whose value is itself.
selfSymbol :: SEXP R.Char -> IO (SEXP R.Symbol)
selfSymbol pname = do
    let s = unhexp $ Symbol pname nullPtr Nothing
    R.setCdr s s
    return s
