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
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.HExp
  ( HExp(..)
  , hexp
  , unhexp
  , vector
  , selfSymbol
  , heqMaybe
  ) where

import Control.Applicative
import H.Internal.Prelude
import qualified Language.R.Globals as H
import qualified Foreign.R      as R
import qualified Foreign.R.Type as R
import           Foreign.R (SEXPREC, withProtected)

import qualified Data.Vector.SEXP as Vector

import Control.Monad ( (<=<) )
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign.C
import Foreign ( castPtr, nullPtr )
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
data HExp :: * -> SEXPTYPE -> * where
  -- Primitive types. The field names match those of <RInternals.h>.
  Nil       :: HExp s R.Nil
  -- Fields: pname, value, internal.
  Symbol    :: SEXP s R.Char
            -> SEXP s a
            -> Maybe (SEXP s b)
            -> HExp s R.Symbol
  -- Fields: carval, cdrval, tagval.
  List      :: SEXP s a
            -> !(Maybe (SEXP s R.List))
            -> !(Maybe (SEXP s R.Symbol))
            -> HExp s R.List
  -- Fields: frame, enclos, hashtab.
  Env       :: SEXP s R.PairList
            -> SEXP s R.Env
            -> SEXP s R.Vector
            -> HExp s R.Env
  -- Fields: formals, body, env.
  Closure   :: SEXP s R.PairList
            -> SEXP s a
            -> SEXP s R.Env
            -> HExp s R.Closure
  -- Fields: value, expr, env.
  Promise   :: SEXP s a
            -> SEXP s b
            -> SEXP s R.Env
            -> HExp s R.Promise
  -- Derived types. These types don't have their own 'struct' declaration in
  -- <Rinternals.h>.
  -- Fields: function, args.
  Lang      :: () -- (a :∈ R.Symbol :+: R.Lang)         -- XXX R.Closure also?
            => SEXP s a
            -> Maybe (SEXP s R.List)
            -> HExp s R.Lang
  -- Fields: offset.
  Special   :: {-# UNPACK #-} !Int32
            -> HExp s R.Special
  -- Fields: offset.
  Builtin   :: {-# UNPACK #-} !Int32
            -> HExp s R.Builtin
  Char      :: {-# UNPACK #-} !(Vector.Vector s R.Char Word8)
            -> HExp s R.Char
  Logical   :: {-# UNPACK #-} !(Vector.Vector s 'R.Logical R.Logical)
            -> HExp s 'R.Logical
  Int       :: {-# UNPACK #-} !(Vector.Vector s R.Int Int32)
            -> HExp s R.Int
  Real      :: {-# UNPACK #-} !(Vector.Vector s R.Real Double)
            -> HExp s R.Real
  Complex   :: {-# UNPACK #-} !(Vector.Vector s R.Complex (Complex Double))
            -> HExp s R.Complex
  String    :: {-# UNPACK #-} !(Vector.Vector s R.String (SEXP s R.Char))
            -> HExp s R.String
  -- Fields: pairlist of promises.
  DotDotDot :: SEXP s R.PairList
            -> HExp s R.List
  -- Fields: truelength, content.
  Vector    :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.Vector s R.Vector (SomeSEXP s))
            -> HExp s R.Vector
  -- Fields: truelength, content.
  Expr      :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.Vector s R.Expr (SomeSEXP s))
            -> HExp s R.Expr
  Bytecode  :: HExp s a -- XXX
  -- Fields: pointer, protectionValue, tagval
  ExtPtr    :: Ptr ()
            -> SEXP s b
            -> SEXP s R.Symbol
            -> HExp s R.ExtPtr
  -- Fields: key, value, finalizer, next.
  WeakRef   :: -- (a :∈ R.Nil :+: R.Env :+: R.ExtPtr
               -- ,c :∈ R.Nil :+: R.Closure :+: R.Builtin :+: R.Special)
               SEXP s a
            -> SEXP s b
            -> SEXP s c
            -> SEXP s d
            -> HExp s R.WeakRef
  Raw       :: {-# UNPACK #-} !(Vector.Vector s R.Raw Word8)
            -> HExp s R.Raw
  -- Fields: tagval.
  S4        :: SEXP s a
            -> HExp s R.S4

deriving instance Show (HExp s a)

-- | Wrapper for partially applying a type synonym.
newtype E s a = E (SEXP s a)

instance HEq (E s) where
  E x@(hexp -> t1) === E y@(hexp -> t2) = R.unsexp x == R.unsexp y || t1 === t2

heqMaybe :: Maybe (SEXP s a) -> Maybe (SEXP s b) -> Bool
heqMaybe (Just x) (Just y) = E x === E y
heqMaybe Nothing Nothing = True
heqMaybe (Just nl) Nothing  | R.unsexp nl == R.unsexp H.nilValue  = True
heqMaybe Nothing  (Just nl) | R.unsexp nl == R.unsexp H.nilValue  = True
heqMaybe _ _ = False

instance HEq (HExp s) where
  Nil === Nil = True
  Symbol pname1 value1 internal1 === Symbol pname2 value2 internal2 =
      E pname1 === E pname2 &&
      E value1 === E value2 &&
      heqMaybe internal1 internal2
  List carval1 cdrval1 tagval1 === List carval2 cdrval2 tagval2 =
      E carval1 === E carval2 &&
      heqMaybe cdrval1 cdrval2 &&
      heqMaybe tagval1 tagval2
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

instance Storable (HExp s a) where
  sizeOf _ = {#sizeof SEXPREC #}
  alignment _ = {#alignof SEXPREC #}
  poke = pokeHExp
  peek = peekHExp . R.SEXP
  {-# INLINE peek #-}

{-# INLINE peekHExp #-}
peekHExp :: SEXP s a -> IO (HExp s a)
peekHExp s = do
    let coerce :: IO (HExp s a) -> IO (HExp s b)
        coerce = unsafeCoerce

        sptr = R.unsexp s

    case R.typeOf s of
      R.Nil       -> coerce $ return Nil
      R.Symbol    -> coerce $
        Symbol    <$> (R.sexp <$> {#get SEXP->u.symsxp.pname #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.symsxp.value #} sptr)
                  <*> (maybeNil =<< R.sexp <$> {#get SEXP->u.symsxp.internal #} sptr)
      R.List      -> coerce $
        List      <$> (R.sexp <$> {#get SEXP->u.listsxp.carval #} sptr)
                  <*> (maybeNil =<< R.sexp <$> {#get SEXP->u.listsxp.cdrval #} sptr)
                  <*> (maybeNil =<< R.sexp <$> {#get SEXP->u.listsxp.tagval #} sptr)
      R.Env       -> coerce $
        Env       <$> (R.sexp <$> {#get SEXP->u.envsxp.frame #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.envsxp.enclos #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.envsxp.hashtab #} sptr)
      R.Closure   -> coerce $
        Closure   <$> (R.sexp <$> {#get SEXP->u.closxp.formals #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.closxp.body #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.closxp.env #} sptr)
      R.Promise   -> coerce $
        Promise   <$> (R.sexp <$> {#get SEXP->u.promsxp.value #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.promsxp.expr #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.promsxp.env #} sptr)
      R.Lang      -> coerce $
        Lang      <$> (R.sexp <$> {#get SEXP->u.listsxp.carval #} sptr)
                  <*> (maybeNil =<< R.sexp <$> {#get SEXP->u.listsxp.cdrval #} sptr)
      R.Special   -> coerce $
        Special   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} sptr)
      R.Builtin   -> coerce $
        Builtin   <$> (fromIntegral <$> {#get SEXP->u.primsxp.offset #} sptr)
      R.Char      -> coerce $ Char    <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Logical   -> coerce $ Logical <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Int       -> coerce $ Int     <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Real      -> coerce $ Real    <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Complex   -> coerce $ Complex <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.String    -> coerce $ String  <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.DotDotDot -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)
      R.Vector    -> coerce $
        Vector    <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} sptr)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Expr      -> coerce $
        Expr      <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} sptr)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Bytecode  -> return $ Bytecode
      R.ExtPtr    -> coerce $
        ExtPtr    <$> (castPtr <$> {#get SEXP->u.listsxp.carval #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} sptr)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.tagval #} sptr)
      R.WeakRef   -> coerce $
        WeakRef   <$> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 0)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 1)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 2)
                  <*> (R.sexp <$> peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 3)
      R.Raw       -> coerce $ Raw     <$> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.S4        -> coerce $
        S4        <$> (R.sexp <$> {# get SEXP->u.listsxp.tagval #} sptr)
      _           -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)

pokeHExp :: Ptr (HExp s a) -> HExp s a -> IO ()
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
hexp :: SEXP s a -> HExp s a
hexp = unsafeInlineIO . peek . R.unSEXP
{-# INLINE hexp #-}

-- | Inverse hexp view to the real structure, note that for scalar types
-- hexp will allocate new SEXP, and @unhexp . hexp@ is not an identity function.
-- however for vector types it will return original SEXP.
unhexp :: MonadR m => HExp (Region m) a -> m (SEXP (Region m) a)
unhexp   Nil = return $ R.release H.nilValue
unhexp s@(Symbol{}) = io $
  withProtected (R.allocSEXP R.SSymbol)
                (\x -> poke (R.unSEXP x) s >> return x)
unhexp (List c md mt) = acquire <=< io $ do
  rc <- R.protect c
  rd <- R.protect $ maybe (R.unsafeCoerce $ R.unsafeRelease $ H.nilValue) id md
  rt <- R.protect $ maybe (R.unsafeCoerce $ R.unsafeRelease $ H.nilValue) id mt
  z  <- R.cons rc rd
  {# set SEXP-> u.listsxp.tagval #} (R.unsexp z) (R.unsexp rt)
  R.unprotect 3
  return z
unhexp (Lang carval mbcdrval) = acquire <=< io $ do
    carval' <- R.protect carval
    cdrval' <- R.protect $
      maybe (R.unsafeCoerce $ R.unsafeRelease H.nilValue) id mbcdrval
    x <- R.allocSEXP R.SLang
    R.setCar x (R.release carval')
    R.setCdr x (R.release cdrval')
    R.unprotect 2
    return x
unhexp s@(Env{})     = io $
    withProtected (R.allocSEXP R.SEnv)
                  (\x -> poke (R.unSEXP x) s >> return x)
unhexp s@(Closure{}) = io $
    withProtected (R.allocSEXP R.SClosure)
                  (\x -> poke (R.unSEXP x) s >> return x)
unhexp s@(Special{}) = io $
    withProtected (R.allocSEXP R.SSpecial)
                  (\x -> poke (R.unSEXP x) s >> return x)
unhexp s@(Builtin{}) = io $
    withProtected (R.allocSEXP R.SBuiltin)
                  (\x -> poke (R.unSEXP x) s >> return x)
unhexp s@(Promise{}) = io $
    withProtected (R.allocSEXP R.SPromise)
                  (\x -> poke (R.unSEXP x) s >> return x)
unhexp  (Bytecode{}) = unimplemented "unhexp"
unhexp (Real vt)     = io $ Vector.unsafeToSEXP vt
unhexp (Logical vt)  = io $ Vector.unsafeToSEXP vt
unhexp (Int vt)      = io $ Vector.unsafeToSEXP vt
unhexp (Complex vt)  = io $ Vector.unsafeToSEXP vt
unhexp (Vector _ vt) = io $ Vector.unsafeToSEXP vt
unhexp (Char vt)     = io $ Vector.unsafeToSEXP vt
unhexp (String vt)   = io $ Vector.unsafeToSEXP vt
unhexp (Raw vt)      = io $ Vector.unsafeToSEXP vt
unhexp S4{}          = unimplemented "unhexp"
unhexp (Expr _ vt)   = io $ Vector.unsafeToSEXP vt
unhexp WeakRef{}     = io $ error "unhexp does not support WeakRef, use Foreign.R.mkWeakRef instead."
unhexp DotDotDot{}   = unimplemented "unhexp"
unhexp ExtPtr{}      = unimplemented "unhexp"

-- | Project the vector out of 'SEXP's.
vector :: R.IsVector a => SEXP s a -> Vector.Vector s a (Vector.ElemRep s a)
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
maybeNil :: SEXP s a
         -> IO (Maybe (SEXP s a))
maybeNil s = do
  return $
    if R.unsexp s == R.unsexp H.nilValue
      then Nothing
      else Just s

-- | Symbols can have values attached to them. This function creates a symbol
-- whose value is itself.
selfSymbol :: SEXP s R.Char -> IO (SEXP s R.Symbol)
selfSymbol pname = unsafeRToIO $ do
    s <- unhexp $ Symbol pname (R.sexp nullPtr) Nothing
    io $ R.setCdr s s
    return s
