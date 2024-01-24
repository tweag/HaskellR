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

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--prune-unsorted" @-}
module Language.R.HExp
  ( HExp(..)
  , hexp
  , htypeOf
  , vector
  ) where

import Control.Applicative
import Control.Memory.Region (V)
import qualified Foreign.R      as R
import Foreign.R (SEXP, SEXPTYPE)
import Internal.Error

import qualified Data.Vector.SEXP as Vector

import Control.Memory.Region -- XXX: Needed for LH name resolution
import Control.Monad.Primitive ( unsafeInlineIO )
import Control.Monad.R.Class -- XXX: imported to help LH name resolution
import Control.Monad.ST -- XXX: Needed for LH name resolution
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import Data.Kind (Type)
import qualified Data.Vector.SEXP.Mutable as Mutable -- XXX: Needed for LH name resolution
import GHC.Ptr (Ptr(..))
import Foreign.C.String -- XXX: Needed for LH name resolution
import Foreign.ForeignPtr -- XXX: imported to help LH name resolution
import Foreign.Storable
import Foreign (castPtr)
-- Fixes redundant import warning >= 7.10 without CPP
import Prelude


-- Use explicit UNPACK pragmas rather than -funbox-strict-fields in order to get
-- warnings if a field is not unpacked when we expect it to.

-- | A view of R's internal 'SEXP' structure as an algebraic datatype. Because
-- this is in fact a GADT, the use of named record fields is not possible here.
-- Named record fields give rise to functions for whom it is not possible to
-- assign a reasonable type (existentially quantified type variables would
-- escape).
--
-- See <https://cran.r-project.org/doc/manuals/r-release/R-ints.html#SEXPTYPEs>.
type role HExp phantom
data HExp :: Type -> Type where
  -- Primitive types. The field names match those of <RInternals.h>.
  -- | The NULL value (@NILSXP@).
  Nil       :: HExp s
  -- | A symbol (@SYMSXP@).
  Symbol    :: SEXP s -- ^ the name (is 'Nil' for 'H.unboundValue')
            -> SEXP s -- ^ the value. Many symbols have their value set to 'H.unboundValue'.
            -> SEXP s -- ^ «internal»: if the symbol's value is a @.Internal@ function,
                        -- this is a pointer to the appropriate 'SEXP'.
            -> HExp s
  -- | A list (@LISTSXP@).
  List      :: SEXP s -- ^ CAR
            -> SEXP s -- ^ CDR (usually a 'List' or 'Nil')
            -> SEXP s -- ^ TAG (a 'Symbol' or 'Nil')
            -> HExp s
  -- | An environment (@ENVSXP@).
  Env       :: SEXP s -- ^ the frame: a tagged pairlist with tag the symbol and CAR the bound value
            -> SEXP s -- ^ the enclosing environment
            -> SEXP s -- ^ the hash table
            -> HExp s
  -- | A closure (@CLOSXP@).
  Closure   :: SEXP s -- ^ formals (a pairlist)
            -> SEXP s -- ^ the body
            -> SEXP s -- ^ the environment
            -> HExp s
  -- | A promise (@PROMSXP@).
  Promise   :: SEXP s -- ^ the value
            -> SEXP s -- ^ the expression
            -> SEXP s -- ^ the environment. Once the promise has been
                        -- evaluated, the environment is set to NULL.
            -> HExp s
  -- Derived types. These types don't have their own 'struct' declaration in
  -- <Rinternals.h>.
  -- | Language objects (@LANGSXP@) are calls (including formulae and so on).
  -- Internally they are pairlists with first element a reference to the
  -- function to be called with remaining elements the actual arguments for
  -- the call (and with the tags if present giving the specified argument
  -- names). Although this is not enforced, many places in the R code assume
  -- that the pairlist is of length one or more, often without checking.
  Lang      :: SEXP s -- ^ CAR: the function (perhaps via a symbol or language object)
            -> SEXP s -- ^ CDR: the argument list with tags for named arguments
            -> HExp s
  -- | A special (built-in) function call (@SPECIALSXP@). It carries an offset
  -- into the table of primitives but for our purposes is opaque.
  Special   :: HExp s
  -- | A @BUILTINSXP@. This is similar to 'Special', except the arguments to a 'Builtin'
  -- are always evaluated.
  Builtin   :: HExp s
  -- | An internal character string (@CHARSXP@).
  Char      :: Vector.Vector Word8
            -> HExp s
  -- | A logical vector (@LGLSXP@).
  Logical   :: Vector.Vector R.Logical
            -> HExp s
  -- | An integer vector (@INTSXP@).
  Int       :: Vector.Vector Int32
            -> HExp s
  -- | A numeric vector (@REALSXP@).
  Real      :: Vector.Vector Double
            -> HExp s
  -- | A complex vector (@CPLXSXP@).
  Complex   :: Vector.Vector (Complex Double)
            -> HExp s
  -- | A character vector (@STRSXP@).
  String    :: Vector.Vector (SEXP V)
            -> HExp s
  -- | A special type of @LISTSXP@ for the value bound to a @...@ symbol
  DotDotDot :: SEXP s -- ^ a pairlist of promises
            -> HExp s
  -- | A list/generic vector (@VECSXP@).
  Vector    :: Int32 -- ^ true length
            -> Vector.Vector (SEXP V)
            -> HExp s
  -- | An expression vector (@EXPRSXP@).
  Expr      :: Int32 -- ^ true length
            -> Vector.Vector (SEXP V)
            -> HExp s
  -- | A ‘byte-code’ object generated by R (@BCODESXP@).
  Bytecode  :: HExp s -- TODO
  -- | An external pointer (@EXTPTRSXP@)
  ExtPtr    :: Ptr () -- ^ the pointer
            -> SEXP s -- ^ the protection value (an R object which if alive protects this object)
            -> SEXP s -- ^ a tag
            -> HExp s
  -- | A weak reference (@WEAKREFSXP@).
  WeakRef   :: SEXP s -- ^ the key
            -> SEXP s -- ^ the value
            -> SEXP s -- ^ the finalizer
            -> SEXP s -- ^ the next entry in the weak references list
            -> HExp s
  -- | A raw vector (@RAWSXP@).
  Raw       :: Vector.Vector Word8
            -> HExp s
  -- | An S4 class which does not consist solely of a simple type such as an atomic vector or function (@S4SXP@).
  S4        :: SEXP s -- ^ the tag
            -> HExp s

{-@
type THExp s T = {e:HExp s | htypeOf e == T }
@-}

{-@ reflect htypeOf @-}
htypeOf :: HExp s -> SEXPTYPE
htypeOf = \case
    Nil -> R.Nil
    Symbol{} -> R.Symbol
    List{} -> R.List
    Env{} -> R.Env
    Closure{} -> R.Closure
    Promise{} -> R.Promise
    Lang{} -> R.Lang
    Special{} -> R.Special
    Builtin{} -> R.Builtin
    Char{} -> R.SChar
    Int{} -> R.SInt
    Logical{} -> R.Logical
    Real{} -> R.Real
    Complex{} -> R.SComplex
    String{} -> R.SString
    DotDotDot{} -> R.List
    Vector{} -> R.SVector
    Expr{} -> R.Expr
    Bytecode{} -> R.Bytecode
    ExtPtr{} -> R.ExtPtr
    WeakRef{} -> R.WeakRef
    Raw{} -> R.Raw
    S4{} -> R.S4

{-@
data HExp :: * -> * where
  Nil       :: HExp s
  Symbol    :: {e1:SEXP s| typeOf e1 == R.SChar || typeOf e1 == R.Nil}
            -> SEXP s
            -> SEXP s
            -> HExp s
  List      :: SEXP s
            -> {e2:SEXP s | typeOf e2 == R.List || typeOf e2 == R.Nil}
            -> {e3:SEXP s | typeOf e3 == R.Symbol || typeOf e3 == R.Nil}
            -> HExp s
  Env       :: {e1:SEXP s | typeOf e1 == R.List || typeOf e1 == R.Nil}
            -> {e2:SEXP s | typeOf e2 == R.Env || typeOf e2 == R.Nil}
            -> {e3:SEXP s | typeOf e3 == R.SVector || typeOf e3 == R.Nil}
            -> HExp s
  Closure   :: {e1:SEXP s | typeOf e1 == R.List || typeOf e1 == R.Nil}
            -> SEXP s
            -> TSEXP s R.Env
            -> HExp s
  Promise   :: SEXP s
            -> {e2:SEXP s | typeOf e2 == R.Lang || typeOf e2 == R.Expr || typeOf e2 == R.Symbol}
            -> {e3:SEXP s | typeOf e3 == R.Env || typeOf e3 == R.Nil}
            -> HExp s
  Lang      :: {e1:SEXP s | typeOf e1 == R.Lang || typeOf e1 == R.Expr || typeOf e1 == R.Symbol}
            -> {e2:SEXP s | typeOf e2 == R.List || typeOf e2 == R.Nil}
            -> HExp s
  Special   :: HExp s
  Builtin   :: HExp s
  Char      :: TVector Word8 R.SChar
            -> HExp s
  Logical   :: TVector Foreign.R.Context.Logical R.Logical
            -> HExp s
  Int       :: TVector Int32 R.SInt
            -> HExp s
  Real      :: TVector Double R.Real
            -> HExp s
  Complex   :: TVector (Complex Double) R.SComplex
            -> HExp s
  String    :: TVector (TSEXP V R.SChar) R.SString
            -> HExp s
  DotDotDot :: {e1:SEXP s | typeOf e1 == R.List || typeOf e1 == R.Nil}
            -> HExp s
  Vector    :: Int32
            -> TVector (SEXP V) R.SVector
            -> HExp s
  Expr      :: Int32
            -> TVector (SEXP V) R.Expr
            -> HExp s
  Bytecode  :: HExp s
  ExtPtr    :: Ptr ()
            -> SEXP s
            -> {e2:SEXP s | typeOf e2 == R.Symbol || typeOf e2 == R.Nil}
            -> HExp s
  WeakRef   :: {e1:SEXP s | typeOf e1 == R.Env || typeOf e1 == R.ExtPtr || typeOf e1 == R.Nil}
            -> SEXP s
            -> {e3:SEXP s | typeOf e3 == R.Closure || typeOf e3 == R.Builtin || typeOf e3 == R.Special || typeOf e3 == R.Nil}
            -> {e4:SEXP s | typeOf e4 == R.WeakRef || typeOf e4 == R.Nil}
            -> HExp s
  Raw       :: TVector Word8 R.Raw
            -> HExp s
  S4        :: {e1:SEXP s | typeOf e1 == R.Symbol || typeOf e1 == R.Nil}
            -> HExp s
@-}

-- | Wrapper for partially applying a type synonym.
newtype E s = E (SEXP s)

instance Eq (E s) where
  (E x@(hexp -> t1)) == (E y@(hexp -> t2)) =
      R.unsexp x == R.unsexp y || t1 == t2

instance Eq (HExp s) where
  (==) Nil Nil = True
  (==) (Symbol pname1 value1 internal1) (Symbol pname2 value2 internal2) =
         E pname1 == E pname2
      && E value1 == E value2
      && E internal1 == E internal2
  (==) (List carval1 cdrval1 tagval1) (List carval2 cdrval2 tagval2) =
         E carval1 == E carval2
      && E cdrval1 == E cdrval2
      && E tagval1 == E tagval2
  (==) (Env frame1 enclos1 hashtab1) (Env frame2 enclos2 hashtab2) =
         E frame1 == E frame2
      && E enclos1 == E enclos2
      && E hashtab1 == E hashtab2
  (==) (Closure formals1 body1 env1) (Closure formals2 body2 env2) =
         E formals1 == E formals2
      && E body1 == E body2
      && E env1 == E env2
  (==) (Promise value1 expr1 env1) (Promise value2 expr2 env2) =
         E value1 == E value2
      && E expr1 == E expr2
      && E env1 == E env2
  (==) (Lang carval1 cdrval1) (Lang carval2 cdrval2) =
         E carval1 == E carval2
      && E cdrval1 == E cdrval2
  -- Not comparable
  (==) Special Special = False
  -- Not comparable
  (==) Builtin Builtin = False
  (==) (Char vec1) (Char vec2) =
      vec1 == vec2
  (==) (Int vec1) (Int vec2) =
      vec1 == vec2
  (==) (Real vec1) (Real vec2) =
      vec1 == vec2
  (==) (String vec1) (String vec2) =
      vec1 == vec2
  (==) (Complex vec1) (Complex vec2) =
      vec1 == vec2
  (==) (DotDotDot pairlist1) (DotDotDot pairlist2) =
      E pairlist1 == E pairlist2
  (==) (Vector truelength1 vec1) (Vector truelength2 vec2) =
      let eq s1 s2 = E s1 == E s2
       in truelength1 == truelength2
          && and (zipWith eq (Vector.toList vec1) (Vector.toList vec2))
  (==) (Expr truelength1 vec1) (Expr truelength2 vec2) =
      let eq s1 s2 = E s1 == E s2
       in truelength1 == truelength2
          && and (zipWith eq (Vector.toList vec1) (Vector.toList vec2))
  (==) Bytecode Bytecode = True
  (==) (ExtPtr pointer1 protectionValue1 tagval1) (ExtPtr pointer2 protectionValue2 tagval2) =
         castPtr pointer1 == castPtr pointer2
      && E protectionValue1 == E protectionValue2
      && E tagval1 == E tagval2
  (==) (WeakRef key1 value1 finalizer1 next1) (WeakRef key2 value2 finalizer2 next2) =
         E key1 == E key2
      && E value1 == E value2
      && E finalizer1 == E finalizer2
      && E next1 == E next2
  (==) (Raw vec1) (Raw vec2) =
      vec1 == vec2
  (==) (S4 tagval1) (S4 tagval2) =
      E tagval1 == E tagval2
  (==) _ _ = False


{-# INLINE peekHExp #-}
{-@ assume peekHExp :: x:SEXP s -> IO (THExp s (typeOf x)) @-}
{-@ ignore peekHExp @-}
peekHExp :: SEXP s -> IO (HExp s)
peekHExp s =
    case R.typeOf s of
      R.Nil       -> return Nil
      R.Symbol    ->
        Symbol    <$> R.symbolPrintName s
                  <*> R.symbolValue s
                  <*> R.symbolInternal s
      R.List      ->
        List      <$> R.car s
                  <*> R.cdr s
                  <*> R.tag s
      R.Env       ->
        Env       <$> R.envFrame s
                  <*> R.envEnclosing s
                  <*> R.envHashtab s
      R.Closure   ->
        Closure   <$> R.closureFormals s
                  <*> R.closureBody s
                  <*> R.closureEnv s
      R.Promise   ->
        Promise   <$> R.promiseValue s
                  <*> R.promiseCode s
                  <*> R.promiseEnv s
      R.Lang      ->
        Lang      <$> R.car s
                  <*> R.cdr s
      R.Special   -> return Special
      R.Builtin   -> return Builtin
      R.SChar     -> return $ Char    (Vector.unsafeFromSEXP s)
      R.Logical   -> return $ Logical (Vector.unsafeFromSEXP s)
      R.SInt      -> return $ Int     (Vector.unsafeFromSEXP s)
      R.Real      -> return $ Real    (Vector.unsafeFromSEXP s)
      R.SComplex  -> return $ Complex (Vector.unsafeFromSEXP s)
      R.SString   -> return $ String  (Vector.unsafeFromSEXP s)
      R.DotDotDot -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)
      R.SVector   ->
        Vector    <$> (fromIntegral <$> R.trueLength s)
                  <*> pure (Vector.unsafeFromSEXP s)
      R.Expr      ->
        Expr      <$> (fromIntegral <$> R.trueLength s)
                  <*> pure (Vector.unsafeFromSEXP s)
      R.Bytecode  -> return Bytecode
      R.ExtPtr    ->
        ExtPtr    <$> ((\(R.SEXP (R.SEXP0 ptr)) -> castPtr ptr) <$> R.car s)
                  <*> R.cdr s
                  <*> R.tag s
      R.WeakRef   ->
        WeakRef   <$> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 0)
                  <*> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 1)
                  <*> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 2)
                  <*> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 3)
      R.Raw       -> return $ Raw (Vector.unsafeFromSEXP s)
      R.S4        ->
        S4        <$> R.tag s
      _           -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
{-@ assume hexp :: x:SEXP s -> THExp s (R.typeOf x) @-}
hexp :: SEXP s -> HExp s
hexp = unsafeInlineIO . peekHExp
{-# INLINE hexp #-}

-- | Project the vector out of 'SEXP's.
{-@ assume vector :: vt:VSEXPTYPE V a -> {x:SEXP s | vstypeOf vt == R.typeOf x} -> TVector a (vstypeOf vt) @-}
vector :: Vector.VSEXPTYPE V a -> SEXP s -> Vector.Vector a
vector Vector.VChar (hexp -> Char vec)     = vec
vector Vector.VLogical (hexp -> Logical vec)  = vec
vector Vector.VInt (hexp -> Int vec)      = vec
vector Vector.VReal (hexp -> Real vec)     = vec
vector Vector.VComplex (hexp -> Complex vec)  = vec
vector Vector.VString (hexp -> String vec)   = vec
vector Vector.VVector (hexp -> Vector _ vec) = vec
vector Vector.VExpr (hexp -> Expr _ vec)   = vec
vector _ s = violation "vector" $ show (R.typeOf s) ++ " unexpected vector type."
