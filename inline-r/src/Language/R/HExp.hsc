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
  , unhexp
  , vector
  ) where

import Control.Applicative
import Control.Memory.Region (V)
import Control.Monad.R.Class
import qualified Foreign.R      as R
import Foreign.R (SEXP, SomeSEXP(..), SEXPTYPE, withProtected)
import Foreign.R.Constraints
import Internal.Error
import qualified Language.R.Globals as H

import qualified Data.Vector.SEXP as Vector

import Control.Monad ((<=<), guard, void)
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import Data.Maybe (isJust)
import Data.Type.Equality (TestEquality(..), (:~:)(Refl))
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign.C -- for hsc2hs
import Foreign (castPtr)
import Unsafe.Coerce (unsafeCoerce)
-- Fixes redundant import warning >= 7.10 without CPP
import Prelude

#define USE_RINTERNALS
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
  -- | A special (built-in) function call (@SPECIALSXP@).
  Special   :: {-# UNPACK #-} !Int32
              -- ^ An integer giving the offset into the table of
              -- primitives/@.Internal@s.
            -> HExp s 'R.Special
  -- | A @BUILTINSXP@. This is similar to 'Special', except the arguments to a 'Builtin'
  -- are always evaluated.
  Builtin   :: {-# UNPACK #-} !Int32
              -- ^ An integer giving the offset into the table of
              -- primitives/@.Internal@s.
            -> HExp s 'R.Builtin
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
  ExtPtr    :: Ptr () -- ^ the pointer
            -> SEXP s b -- ^ the protection value (an R object which if alive protects this object)
            -> SEXP s 'R.Symbol -- ^ a tag
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
  S4        :: SEXP s a -- ^ the tag
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
  testEquality (Special offset1) (Special offset2) = do
      guard $ offset1 == offset2
      return Refl
  testEquality (Builtin offset1) (Builtin offset2) = do
      guard $ offset1 == offset2
      return Refl
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

instance Storable (HExp s a) where
  sizeOf _ = #{size SEXPREC}
  alignment _ = #{alignment SEXPREC}
  poke = pokeHExp
  peek = peekHExp . R.SEXP
  {-# INLINE peek #-}

{-# INLINE peekHExp #-}
peekHExp :: SEXP s a -> IO (HExp s a)
peekHExp s = do
    let coerce :: IO (HExp s a) -> IO (HExp s b)
        coerce = unsafeCoerce

        -- (:∈) constraints are impossible to respect in 'peekHExp', because
        -- R doesn't tell us statically the form of the SEXPREC referred to by
        -- a pointer. So in this function only, we pretend all constrained
        -- fields actually always contain fields of form ANYSXP. This has no
        -- operational significance - it's only a way to bypass what's
        -- impossible to prove.
        coerceAny :: SEXP s a -> SEXP s 'R.Any -- '
        coerceAny = R.unsafeCoerce

        sptr = R.unsexp s

    case R.typeOf s of
      R.Nil       -> coerce $ return Nil
      R.Symbol    -> coerce $
        Symbol    <$> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.symsxp.pname} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.symsxp.value} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.symsxp.internal} sptr)
      R.List      -> coerce $
        List      <$> (R.sexp <$> #{peek SEXPREC, u.listsxp.carval} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.listsxp.cdrval} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.listsxp.tagval} sptr)
      R.Env       -> coerce $
        Env       <$> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.envsxp.frame} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.envsxp.enclos} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.envsxp.hashtab} sptr)
      R.Closure   -> coerce $
        Closure   <$> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.closxp.formals} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.closxp.body} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.closxp.env} sptr)
      R.Promise   -> coerce $
        Promise   <$> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.promsxp.value} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.promsxp.expr} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.promsxp.env} sptr)
      R.Lang      -> coerce $
        Lang      <$> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.listsxp.carval} sptr)
                  <*> (coerceAny <$> R.sexp <$> #{peek SEXPREC, u.listsxp.cdrval} sptr)
      R.Special   -> coerce $
        Special   <$> (fromIntegral <$> (#{peek SEXPREC, u.primsxp.offset} sptr :: IO CInt))
      R.Builtin   -> coerce $
        Builtin   <$> (fromIntegral <$> (#{peek SEXPREC, u.primsxp.offset} sptr :: IO CInt))
      R.Char      -> unsafeCoerce $ Char    (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Logical   -> unsafeCoerce $ Logical (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Int       -> unsafeCoerce $ Int     (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Real      -> unsafeCoerce $ Real    (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Complex   -> unsafeCoerce $ Complex (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.String    -> unsafeCoerce $ String  (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.DotDotDot -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)
      R.Vector    -> coerce $
        Vector    <$> (fromIntegral <$> (#{peek VECTOR_SEXPREC, vecsxp.truelength} sptr :: IO CInt))
                  <*> pure (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Expr      -> coerce $
        Expr      <$> (fromIntegral <$> (#{peek VECTOR_SEXPREC, vecsxp.truelength} sptr :: IO CInt))
                  <*> pure (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.Bytecode  -> coerce $ return Bytecode
      R.ExtPtr    -> coerce $
        ExtPtr    <$> (castPtr <$> #{peek SEXPREC, u.listsxp.carval} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.listsxp.cdrval} sptr)
                  <*> (R.sexp <$> #{peek SEXPREC, u.listsxp.tagval} sptr)
      R.WeakRef   -> coerce $
        WeakRef   <$> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 0)
                  <*> (R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 1)
                  <*> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 2)
                  <*> (coerceAny <$> R.sexp <$>
                       peekElemOff (castPtr $ R.unsafeSEXPToVectorPtr s) 3)
      R.Raw       -> unsafeCoerce $ Raw (Vector.unsafeFromSEXP (unsafeCoerce s))
      R.S4        -> coerce $
        S4        <$> (R.sexp <$> #{peek SEXPREC, u.listsxp.tagval} sptr)
      _           -> unimplemented $ "peekHExp: " ++ show (R.typeOf s)

pokeHExp :: Ptr (HExp s a) -> HExp s a -> IO ()
pokeHExp s h = do
    case h of
         Nil -> return ()
         Symbol pname value internal -> do
           #{poke SEXPREC, u.symsxp.pname} s (R.unsexp pname)
           #{poke SEXPREC, u.symsxp.value} s (R.unsexp value)
           #{poke SEXPREC, u.symsxp.internal} s (R.unsexp internal)
         List carval cdrval tagval -> do
           #{poke SEXPREC, u.listsxp.carval} s (R.unsexp carval)
           #{poke SEXPREC, u.listsxp.cdrval} s (R.unsexp cdrval)
           #{poke SEXPREC, u.listsxp.tagval} s (R.unsexp tagval)
         Env frame enclos hashtab -> do
           #{poke SEXPREC, u.envsxp.frame} s (R.unsexp frame)
           #{poke SEXPREC, u.envsxp.enclos} s (R.unsexp enclos)
           #{poke SEXPREC, u.envsxp.hashtab} s (R.unsexp hashtab)
         Closure formals body env -> do
           #{poke SEXPREC, u.closxp.formals} s (R.unsexp formals)
           #{poke SEXPREC, u.closxp.body} s (R.unsexp body)
           #{poke SEXPREC, u.closxp.env} s (R.unsexp env)
         Promise value expr env -> do
           #{poke SEXPREC, u.promsxp.value} s (R.unsexp value)
           #{poke SEXPREC, u.promsxp.expr} s (R.unsexp expr)
           #{poke SEXPREC, u.promsxp.env} s (R.unsexp env)
         Lang carval cdrval -> do
           #{poke SEXPREC, u.listsxp.carval} s (R.unsexp carval)
           #{poke SEXPREC, u.listsxp.cdrval} s (R.unsexp cdrval)
         Special offset -> do
           #{poke SEXPREC, u.primsxp.offset} s (fromIntegral offset :: CInt)
         Builtin offset -> do
           #{poke SEXPREC, u.primsxp.offset} s (fromIntegral offset :: CInt)
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
unhexp (List carval cdrval tagval) = acquire <=< io $ do
  rc <- R.protect carval
  rd <- R.protect cdrval
  rt <- R.protect tagval
  z  <- R.cons rc rd
  #{poke SEXPREC, u.listsxp.tagval} (R.unsexp z) (R.unsexp rt)
  R.unprotect 3
  return z
unhexp (Lang carval cdrval) = acquire <=< io $ do
    carval' <- R.protect carval
    cdrval' <- R.protect cdrval
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
unhexp (Real vt)     = return $ Vector.unsafeToSEXP vt
unhexp (Logical vt)  = return $ Vector.unsafeToSEXP vt
unhexp (Int vt)      = return $ Vector.unsafeToSEXP vt
unhexp (Complex vt)  = return $ Vector.unsafeToSEXP vt
unhexp (Vector _ vt) = return $ Vector.unsafeToSEXP vt
unhexp (Char vt)     = return $ Vector.unsafeToSEXP vt
unhexp (String vt)   = return $ Vector.unsafeToSEXP vt
unhexp (Raw vt)      = return $ Vector.unsafeToSEXP vt
unhexp S4{}          = unimplemented "unhexp"
unhexp (Expr _ vt)   = return $ Vector.unsafeToSEXP vt
unhexp WeakRef{}     = error "unhexp does not support WeakRef, use Foreign.R.mkWeakRef instead."
unhexp DotDotDot{}   = unimplemented "unhexp"
unhexp ExtPtr{}      = unimplemented "unhexp"

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
