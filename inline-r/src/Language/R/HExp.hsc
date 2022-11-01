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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.R.HExp
  ( HExp(..)
  , (===)
  , hexp
  , vector
  ) where

import Control.Applicative
import Control.Memory.Region (V)
import qualified Foreign.R      as R
import Foreign.R (SEXP, SomeSEXP(..), SEXPTYPE)
import Foreign.R.Constraints
import Internal.Error

import qualified Data.Vector.SEXP as Vector

import Control.Monad (guard, void)
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import Data.Maybe (isJust)
import Data.Type.Equality (TestEquality(..), (:~:)(Refl))
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign (castPtr)
import Unsafe.Coerce (unsafeCoerce)
-- Fixes redundant import warning >= 7.10 without CPP
import Prelude

#include <R.h>
#include <Rinternals.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- Use explicit UNPACK pragmas rather than -funbox-strict-fields in order to get
-- warnings if a field is not unpacked when we expect it to.

-- | A view of R's internal 'SEXP' structure as an algebraic datatype. Because
-- this is in fact a GADT, the use of named record fields is not possible here.
-- Named record fields give rise to functions for whom it is not possible to
-- assign a reasonable type (existentially quantified type variables would
-- escape).
--
-- See <https://cran.r-project.org/doc/manuals/r-release/R-ints.html#SEXPTYPEs>.
type role HExp phantom nominal
data HExp :: * -> SEXPTYPE -> * where
  -- Primitive types. The field names match those of <RInternals.h>.
  -- | The NULL value (@NILSXP@).
  Nil       :: HExp s 'R.Nil
  -- | A symbol (@SYMSXP@).
  Symbol    :: (a :∈ ['R.Char, 'R.Nil])
            => SEXP s a -- ^ the name (is 'Nil' for 'H.unboundValue')
            -> SEXP s b -- ^ the value. Many symbols have their value set to 'H.unboundValue'.
            -> SEXP s c -- ^ «internal»: if the symbol's value is a @.Internal@ function,
                        -- this is a pointer to the appropriate 'SEXP'.
            -> HExp s 'R.Symbol
  -- | A list (@LISTSXP@).
  List      :: (R.IsPairList b, c :∈ ['R.Symbol, 'R.Nil])
            => SEXP s a -- ^ CAR
            -> SEXP s b -- ^ CDR (usually a 'List' or 'Nil')
            -> SEXP s c -- ^ TAG (a 'Symbol' or 'Nil')
            -> HExp s 'R.List
  -- | An environment (@ENVSXP@).
  Env       :: (R.IsPairList a, b :∈ ['R.Env, 'R.Nil], c :∈ ['R.Vector, 'R.Nil])
            => SEXP s a -- ^ the frame: a tagged pairlist with tag the symbol and CAR the bound value
            -> SEXP s b -- ^ the enclosing environment
            -> SEXP s c -- ^ the hash table
            -> HExp s 'R.Env
  -- | A closure (@CLOSXP@).
  Closure   :: (R.IsPairList a)
            => SEXP s a -- ^ formals (a pairlist)
            -> SEXP s b -- ^ the body
            -> SEXP s 'R.Env -- ^ the environment
            -> HExp s 'R.Closure
  -- | A promise (@PROMSXP@).
  Promise   :: (R.IsExpression b, c :∈ ['R.Env, 'R.Nil])
            => SEXP s a -- ^ the value
            -> SEXP s b -- ^ the expression
            -> SEXP s c -- ^ the environment. Once the promise has been
                        -- evaluated, the environment is set to NULL.
            -> HExp s 'R.Promise
  -- Derived types. These types don't have their own 'struct' declaration in
  -- <Rinternals.h>.
  -- | Language objects (@LANGSXP@) are calls (including formulae and so on).
  -- Internally they are pairlists with first element a reference to the
  -- function to be called with remaining elements the actual arguments for
  -- the call (and with the tags if present giving the specified argument
  -- names). Although this is not enforced, many places in the R code assume
  -- that the pairlist is of length one or more, often without checking.
  Lang      :: (R.IsExpression a, R.IsPairList b)
            => SEXP s a -- ^ CAR: the function (perhaps via a symbol or language object)
            -> SEXP s b -- ^ CDR: the argument list with tags for named arguments
            -> HExp s 'R.Lang
  -- | A special (built-in) function call (@SPECIALSXP@). It carries an offset
  -- into the table of primitives but for our purposes is opaque.
  Special   :: HExp s 'R.Special
  -- | A @BUILTINSXP@. This is similar to 'Special', except the arguments to a 'Builtin'
  -- are always evaluated.
  Builtin   :: HExp s 'R.Builtin
  -- | An internal character string (@CHARSXP@).
  Char      :: {-# UNPACK #-} !(Vector.Vector 'R.Char Word8)
            -> HExp s 'R.Char
  -- | A logical vector (@LGLSXP@).
  Logical   :: {-# UNPACK #-} !(Vector.Vector 'R.Logical R.Logical)
            -> HExp s 'R.Logical
  -- | An integer vector (@INTSXP@).
  Int       :: {-# UNPACK #-} !(Vector.Vector 'R.Int Int32)
            -> HExp s 'R.Int
  -- | A numeric vector (@REALSXP@).
  Real      :: {-# UNPACK #-} !(Vector.Vector 'R.Real Double)
            -> HExp s 'R.Real
  -- | A complex vector (@CPLXSXP@).
  Complex   :: {-# UNPACK #-} !(Vector.Vector 'R.Complex (Complex Double))
            -> HExp s 'R.Complex
  -- | A character vector (@STRSXP@).
  String    :: {-# UNPACK #-} !(Vector.Vector 'R.String (SEXP V 'R.Char))
            -> HExp s 'R.String
  -- | A special type of @LISTSXP@ for the value bound to a @...@ symbol
  DotDotDot :: (R.IsPairList a)
            => SEXP s a -- ^ a pairlist of promises
            -> HExp s 'R.List
  -- | A list/generic vector (@VECSXP@).
  Vector    :: {-# UNPACK #-} !Int32 -- ^ true length
            -> {-# UNPACK #-} !(Vector.Vector 'R.Vector (SomeSEXP V))
            -> HExp s 'R.Vector
  -- | An expression vector (@EXPRSXP@).
  Expr      :: {-# UNPACK #-} !Int32 -- ^ true length
            -> {-# UNPACK #-} !(Vector.Vector 'R.Expr (SomeSEXP V))
            -> HExp s 'R.Expr
  -- | A ‘byte-code’ object generated by R (@BCODESXP@).
  Bytecode  :: HExp s 'R.Bytecode -- TODO
  -- | An external pointer (@EXTPTRSXP@)
  ExtPtr    :: (c :∈ ['R.Symbol, 'R.Nil])
            => Ptr () -- ^ the pointer
            -> SEXP s b -- ^ the protection value (an R object which if alive protects this object)
            -> SEXP s c -- ^ a tag
            -> HExp s 'R.ExtPtr
  -- | A weak reference (@WEAKREFSXP@).
  WeakRef   :: ( a :∈ ['R.Env, 'R.ExtPtr, 'R.Nil]
               , c :∈ ['R.Closure, 'R.Builtin, 'R.Special, 'R.Nil]
               , d :∈ ['R.WeakRef, 'R.Nil] )
            => SEXP s a -- ^ the key
            -> SEXP s b -- ^ the value
            -> SEXP s c -- ^ the finalizer
            -> SEXP s d -- ^ the next entry in the weak references list
            -> HExp s 'R.WeakRef
  -- | A raw vector (@RAWSXP@).
  Raw       :: {-# UNPACK #-} !(Vector.Vector 'R.Raw Word8)
            -> HExp s 'R.Raw
  -- | An S4 class which does not consist solely of a simple type such as an atomic vector or function (@S4SXP@).
  S4        :: (a :∈ ['R.Symbol, 'R.Nil])
            => SEXP s a -- ^ the tag
            -> HExp s 'R.S4

-- 'Im a hack

instance Eq (HExp s a) where
  (==) = (===)

-- | Heterogeneous equality.
(===) :: TestEquality f => f a -> f b -> Bool
x === y = isJust $ testEquality x y

-- | Wrapper for partially applying a type synonym.
newtype E s a = E (SEXP s a)

instance TestEquality (E s) where
  testEquality (E x@(hexp -> t1)) (E y@(hexp -> t2)) =
      (guard (R.unsexp x == R.unsexp y) >> return (unsafeCoerce Refl)) <|>
      testEquality t1 t2

instance TestEquality (HExp s) where
  testEquality Nil Nil = return Refl
  testEquality (Symbol pname1 value1 internal1) (Symbol pname2 value2 internal2) = do
      void $ testEquality (E pname1) (E pname2)
      void $ testEquality (E value1) (E value2)
      void $ testEquality (E internal1) (E internal2)
      return Refl
  testEquality (List carval1 cdrval1 tagval1) (List carval2 cdrval2 tagval2) = do
      void $ testEquality (E carval1) (E carval2)
      void $ testEquality (E cdrval1) (E cdrval2)
      void $ testEquality (E tagval1) (E tagval2)
      return Refl
  testEquality (Env frame1 enclos1 hashtab1) (Env frame2 enclos2 hashtab2) = do
      void $ testEquality (E frame1) (E frame2)
      void $ testEquality (E enclos1) (E enclos2)
      void $ testEquality (E hashtab1) (E hashtab2)
      return Refl
  testEquality (Closure formals1 body1 env1) (Closure formals2 body2 env2) = do
      void $ testEquality (E formals1) (E formals2)
      void $ testEquality (E body1) (E body2)
      void $ testEquality (E env1) (E env2)
      return Refl
  testEquality (Promise value1 expr1 env1) (Promise value2 expr2 env2) = do
      void $ testEquality (E value1) (E value2)
      void $ testEquality (E expr1) (E expr2)
      void $ testEquality (E env1) (E env2)
      return Refl
  testEquality (Lang carval1 cdrval1) (Lang carval2 cdrval2) = do
      void $ testEquality (E carval1) (E carval2)
      void $ testEquality (E cdrval1) (E cdrval2)
      return Refl
  -- Not comparable
  testEquality Special Special = Nothing
  -- Not comparable
  testEquality Builtin Builtin = Nothing
  testEquality (Char vec1) (Char vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (Int vec1) (Int vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (Real vec1) (Real vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (String vec1) (String vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (Complex vec1) (Complex vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (DotDotDot pairlist1) (DotDotDot pairlist2) = do
      void $ testEquality (E pairlist1) (E pairlist2)
      return Refl
  testEquality (Vector truelength1 vec1) (Vector truelength2 vec2) = do
      let eq (SomeSEXP s1) (SomeSEXP s2) = isJust $ testEquality (E s1) (E s2)
      guard $ truelength1 == truelength2
      guard $ and $ zipWith eq (Vector.toList vec1) (Vector.toList vec2)
      return Refl
  testEquality (Expr truelength1 vec1) (Expr truelength2 vec2) = do
      let eq (SomeSEXP s1) (SomeSEXP s2) = isJust $ testEquality (E s1) (E s2)
      guard $ truelength1 == truelength2
      guard $ and $ zipWith eq (Vector.toList vec1) (Vector.toList vec2)
      return Refl
  testEquality Bytecode Bytecode = return Refl
  testEquality (ExtPtr pointer1 protectionValue1 tagval1) (ExtPtr pointer2 protectionValue2 tagval2) = do
      guard $ castPtr pointer1 == castPtr pointer2
      void $ testEquality (E protectionValue1) (E protectionValue2)
      void $ testEquality (E tagval1) (E tagval2)
      return Refl
  testEquality (WeakRef key1 value1 finalizer1 next1) (WeakRef key2 value2 finalizer2 next2) = do
      void $ testEquality (E key1) (E key2)
      void $ testEquality (E value1) (E value2)
      void $ testEquality (E finalizer1) (E finalizer2)
      void $ testEquality (E next1) (E next2)
      return Refl
  testEquality (Raw vec1) (Raw vec2) = do
      guard $ vec1 == vec2
      return Refl
  testEquality (S4 tagval1) (S4 tagval2) = do
      void $ testEquality (E tagval1) (E tagval2)
      return Refl
  testEquality _ _ = Nothing

{-# INLINE peekHExp #-}
peekHExp :: forall s a. SEXP s a -> IO (HExp s a)
peekHExp s = do
    let coerce :: IO (HExp s b) -> IO (HExp s c)
        coerce = unsafeCoerce

        -- (:∈) constraints are impossible to respect in 'peekHExp', because
        -- R doesn't tell us statically the form of the SEXPREC referred to by
        -- a pointer. So in this function only, we pretend all constrained
        -- fields actually always contain fields of form ANYSXP. This has no
        -- operational significance - it's only a way to bypass what's
        -- impossible to prove.
        coerceAny :: SEXP s b -> SEXP s 'R.Any -- '
        coerceAny = R.unsafeCoerce

        coerceAnySome :: SomeSEXP s -> SEXP s 'R.Any -- '
        coerceAnySome (SomeSEXP s1) = coerceAny s1

        su :: forall b. SEXP s b
        su = R.unsafeCoerce s

    case R.typeOf s of
      R.Nil       -> coerce $ return Nil
      R.Symbol    -> coerce $
        Symbol    <$> (coerceAnySome <$> R.symbolPrintName su)
                  <*> (coerceAnySome <$> R.symbolValue su)
                  <*> (coerceAnySome <$> R.symbolInternal su)
      R.List      -> coerce $
        List      <$> (coerceAnySome <$> R.car su)
                  <*> (coerceAnySome <$> R.cdr su)
                  <*> (coerceAnySome <$> R.tag su)
      R.Env       -> coerce $
        Env       <$> (coerceAny <$> R.envFrame su)
                  <*> (coerceAny <$> R.envEnclosing su)
                  <*> (coerceAny <$> R.envHashtab su)
      R.Closure   -> coerce $
        Closure   <$> (coerceAny <$> R.closureFormals su)
                  <*> (coerceAnySome <$> R.closureBody su)
                  <*> R.closureEnv su
      R.Promise   -> coerce $
        Promise   <$> (coerceAnySome <$> R.promiseValue su)
                  <*> (coerceAnySome <$> R.promiseCode su)
                  <*> (coerceAnySome <$> R.promiseEnv su)
      R.Lang      -> coerce $
        Lang      <$> (coerceAnySome <$> R.car su)
                  <*> (coerceAnySome <$> R.cdr su)
      R.Special   -> coerce $ return Special
      R.Builtin   -> coerce $ return Builtin
      R.Char      -> unsafeCoerce $ Char    (Vector.unsafeFromSEXP su)
      R.Logical   -> unsafeCoerce $ Logical (Vector.unsafeFromSEXP su)
      R.Int       -> unsafeCoerce $ Int     (Vector.unsafeFromSEXP su)
      R.Real      -> unsafeCoerce $ Real    (Vector.unsafeFromSEXP su)
      R.Complex   -> unsafeCoerce $ Complex (Vector.unsafeFromSEXP su)
      R.String    -> unsafeCoerce $ String  (Vector.unsafeFromSEXP su)
      R.DotDotDot -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)
      R.Vector    -> coerce $
        Vector    <$> (fromIntegral <$> R.trueLength (coerceAny su))
                  <*> pure (Vector.unsafeFromSEXP su)
      R.Expr      -> coerce $
        Expr      <$> (fromIntegral <$> R.trueLength (coerceAny su))
                  <*> pure (Vector.unsafeFromSEXP su)
      R.Bytecode  -> coerce $ return Bytecode
      R.ExtPtr    -> coerce $
        ExtPtr    <$> ((\(R.SomeSEXP (R.SEXP (R.SEXP0 ptr))) -> castPtr ptr) <$> R.car s)
                  <*> (coerceAnySome <$> R.cdr s)
                  <*> (coerceAnySome <$> R.tag s)
      R.WeakRef   -> coerce $
        WeakRef   <$> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 0)
                  <*> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 1)
                  <*> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 2)
                  <*> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 3)
      R.Raw       -> unsafeCoerce $ Raw (Vector.unsafeFromSEXP su)
      R.S4        -> coerce $
        S4        <$> (coerceAnySome <$> R.tag su)
      _           -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
hexp :: SEXP s a -> HExp s a
hexp = unsafeInlineIO . peekHExp
{-# INLINE hexp #-}

-- | Project the vector out of 'SEXP's.
vector :: R.IsVector a => SEXP s a -> Vector.Vector a (Vector.ElemRep V a)
vector (hexp -> Char vec)     = vec
vector (hexp -> Logical vec)  = vec
vector (hexp -> Int vec)      = vec
vector (hexp -> Real vec)     = vec
vector (hexp -> Complex vec)  = vec
vector (hexp -> String vec)   = vec
vector (hexp -> Vector _ vec) = vec
vector (hexp -> Expr _ vec)   = vec
vector s = violation "vector" $ show (R.typeOf s) ++ " unexpected vector type."
