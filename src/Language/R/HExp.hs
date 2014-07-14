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
  , unsafeHexp
  , unhexp
  ) where

import           H.Constraints
import           Foreign.R (SEXP(..), R) 
import qualified Foreign.R.Internal as RI
import qualified Foreign.R as R
import qualified Foreign.R.Type as R

import qualified Language.R.HExp.Unsafe as Unsafe

import qualified Data.Vector.SEXP.Region as Vector

import Control.Applicative
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Complex
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign ( castPtr )

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
data HExp :: * -> R.SEXPTYPE -> * where
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
  Char      :: {-# UNPACK #-} !(Vector.MVector s R.Char Word8)
            -> HExp s R.Char
  Logical   :: {-# UNPACK #-} !(Vector.MVector s 'R.Logical R.Logical)
            -> HExp s 'R.Logical
  Int       :: {-# UNPACK #-} !(Vector.MVector s R.Int Int32)
            -> HExp s R.Int
  Real      :: {-# UNPACK #-} !(Vector.MVector s R.Real Double)
            -> HExp s R.Real
  Complex   :: {-# UNPACK #-} !(Vector.MVector s R.Complex (Complex Double))
            -> HExp s R.Complex
  String    :: {-# UNPACK #-} !(Vector.MVector s R.String (RI.SEXP R.Char))
            -> HExp s R.String
  -- Fields: pairlist of promises.
  DotDotDot :: SEXP s R.PairList
            -> HExp s R.List
  -- Fields: truelength, content.
  Vector    :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.MVector s R.Vector RI.SomeSEXP)
            -> HExp s R.Vector
  -- Fields: truelength, content.
  Expr      :: {-# UNPACK #-} !Int32
            -> {-# UNPACK #-} !(Vector.MVector s R.Expr RI.SomeSEXP)
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
  Raw       :: {-# UNPACK #-} !(Vector.MVector s R.Raw Word8)
            -> HExp s R.Raw
  -- Fields: tagval.
  S4        :: SEXP s a
            -> HExp s R.S4

-- | Wrapper for partially applying a type synonym.
newtype E s a = E (SEXP s a)

instance HEq (E s) where
  E (hexp -> t1) === E (hexp -> t2) = t1 === t2

instance HEq (HExp s) where
  a === b = (toUnsafe a) === (toUnsafe b)

instance Storable (HExp s a) where
  sizeOf x = sizeOf (toUnsafe x)
  alignment x = sizeOf (toUnsafe x) 
  poke p o = poke (castPtr p) (toUnsafe o)
  peek = fmap fromUnsafe . peek . castPtr
  {-# INLINE peek #-}

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
hexp :: SEXP s a -> HExp s a
hexp = fromUnsafe . Unsafe.hexp . unSEXP 
{-# INLINE hexp #-}

unsafeHexp :: SEXP s a -> Unsafe.HExp a
unsafeHexp = Unsafe.hexp . unSEXP
{-# INLINE unsafeHexp #-}

-- | Inverse hexp view to the real structure, note that for scalar types
-- hexp will allocate new SEXP, and @unhexp . hexp@ is not an identity function.
-- however for vector types it will return original SEXP.
unhexp :: HExp s a -> R s (SEXP s a)
unhexp = R.liftProtect . Unsafe.unsafeUnhexp . toUnsafe  

fromUnsafe :: Unsafe.HExp a -> HExp s a
fromUnsafe Unsafe.Nil = Nil
fromUnsafe (Unsafe.Symbol a b c)  = Symbol (SEXP a) (SEXP b) (SEXP <$> c)
fromUnsafe (Unsafe.List a mb mc)  = List (SEXP a) (SEXP <$> mb) (SEXP <$> mc)
fromUnsafe (Unsafe.Env a b c)     = Env  (SEXP a) (SEXP b) (SEXP c)
fromUnsafe (Unsafe.Closure p a r) = Closure (SEXP p) (SEXP a) (SEXP r)
fromUnsafe (Unsafe.Promise a b c) = Promise (SEXP a) (SEXP b) (SEXP c)
fromUnsafe (Unsafe.Lang a b )     = Lang (SEXP a) (SEXP <$> b)
fromUnsafe (Unsafe.Special a)     = Special a
fromUnsafe (Unsafe.Builtin a)     = Builtin a
fromUnsafe (Unsafe.Char a)        = Char (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.Logical a)     = Logical (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.Int a)         = Int (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.Real a)        = Real (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.Complex a)     = Complex (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.String a)      = String (Vector.fromUnsafeI a)
fromUnsafe (Unsafe.DotDotDot a)   = DotDotDot (SEXP a)
fromUnsafe (Unsafe.Vector a b)    = Vector a (Vector.fromUnsafeI b)
fromUnsafe (Unsafe.Expr a b)      = Expr a (Vector.fromUnsafeI b)
fromUnsafe (Unsafe.Bytecode)      = Bytecode
fromUnsafe (Unsafe.ExtPtr a b c)  = ExtPtr a (SEXP b) (SEXP c)
fromUnsafe (Unsafe.WeakRef a b c d) = WeakRef (SEXP a) (SEXP b) (SEXP c) (SEXP d)
fromUnsafe (Unsafe.Raw d)         = Raw (Vector.fromUnsafeI d)
fromUnsafe (Unsafe.S4 a)          = S4 (SEXP a)

toUnsafe :: HExp s a -> Unsafe.HExp a
toUnsafe Nil = Unsafe.Nil
toUnsafe (Symbol a b c)  = Unsafe.Symbol (unSEXP a) (unSEXP b) (unSEXP <$> c)
toUnsafe (List a mb mc)  = Unsafe.List (unSEXP a) (unSEXP <$> mb) (unSEXP <$> mc)
toUnsafe (Env a b c)     = Unsafe.Env  (unSEXP a) (unSEXP b) (unSEXP c)
toUnsafe (Closure p a r) = Unsafe.Closure (unSEXP p) (unSEXP a) (unSEXP r)
toUnsafe (Promise a b c) = Unsafe.Promise (unSEXP a) (unSEXP b) (unSEXP c)
toUnsafe (Lang a b )     = Unsafe.Lang (unSEXP a) (unSEXP <$> b)
toUnsafe (Special a)     = Unsafe.Special a
toUnsafe (Builtin a)     = Unsafe.Builtin a
toUnsafe (Char a)        = Unsafe.Char (Vector.toUnsafeI a)
toUnsafe (Logical a)     = Unsafe.Logical (Vector.toUnsafeI a)
toUnsafe (Int a)         = Unsafe.Int (Vector.toUnsafeI a)
toUnsafe (Real a)        = Unsafe.Real (Vector.toUnsafeI a)
toUnsafe (Complex a)     = Unsafe.Complex (Vector.toUnsafeI a)
toUnsafe (String a)      = Unsafe.String (Vector.toUnsafeI a)
toUnsafe (DotDotDot a)   = Unsafe.DotDotDot (unSEXP a)
toUnsafe (Vector a b)    = Unsafe.Vector a (Vector.toUnsafeI b)
toUnsafe (Expr a b)      = Unsafe.Expr a (Vector.toUnsafeI b)
toUnsafe (Bytecode)      = Unsafe.Bytecode
toUnsafe (ExtPtr a b c)  = Unsafe.ExtPtr a (unSEXP b) (unSEXP c)
toUnsafe (WeakRef a b c d) = Unsafe.WeakRef (unSEXP a) (unSEXP b) (unSEXP c) (unSEXP d)
toUnsafe (Raw d)         = Unsafe.Raw (Vector.toUnsafeI d)
toUnsafe (S4 a)          = Unsafe.S4 (unSEXP a)
