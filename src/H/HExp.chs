-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.HExp
  ( HExp(..)
  , hexp
  , unhexp
  , vector
  , expression
  -- * Low level access
  , injectCar
  , injectCdr
  , injectTag
  , symbolLoop
  ) where

import H.Constraints
import H.Monad
import qualified Foreign.R      as R
import qualified Foreign.R.Type as R
import           Foreign.R (SEXP, SEXPREC, SEXPTYPE)
import qualified Language.R     as LR

import qualified Data.Vector.SEXP as Vector
import           Data.ByteString (ByteString)

import Control.Applicative
import Data.Function (on)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Complex
import GHC.Ptr (Ptr(..))
import Foreign.Storable
import Foreign.C
import Foreign ( castPtr, nullPtr )
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)


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
  Char      :: {-# UNPACK #-} !(Vector.Vector Word8)
            -> HExp (R.Vector Word8)
  Logical   :: {-# UNPACK #-} !(Vector.Vector Bool)
            -> HExp (R.Vector Bool)
  Int       :: {-# UNPACK #-} !(Vector.Vector Int32)
            -> HExp (R.Vector Int32)
  Real      :: {-# UNPACK #-} !(Vector.Vector Double)
            -> HExp (R.Vector Double)
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
            -> {-# UNPACK #-} !(Vector.Vector (SEXP R.Any))
            -> HExp R.Expr
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

-- | Wrapper for partially applying a type synonym.
newtype E a = E (SEXP a)

instance HEq E where
  E (hexp -> t1) === E (hexp -> t2) = t1 === t2

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
      E cdrval1 === E cdrval2
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
      truelength1 == truelength2 &&
      and (zipWith ((===) `on` E) (Vector.toList vec1) (Vector.toList vec2))
  Expr truelength1 vec1 === Expr truelength2 vec2 =
      truelength1 == truelength2 &&
      and (zipWith ((===) `on` E) (Vector.toList vec1) (Vector.toList vec2))
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

peekHExp :: SEXP a -> IO (HExp a)
peekHExp s = do
    let coerce :: IO (HExp a) -> IO (HExp b)
        coerce = unsafeCoerce

{- Will be here until we decide if we will use bytestings or vectors
        bytestring :: IO ByteString
        bytestring = do
          len <- {#get VECSEXP->vecsxp.length #} s
          let !(Ptr addr#) = s `plusPtr` {#sizeof SEXPREC_ALIGN #}
          BS.unsafePackAddressLen (fromIntegral len) addr#
-}

    R.typeOf s >>= \case
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
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s)
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
      R.DotDotDot -> coerce $ error "peekHExp: Unimplemented."
      R.Any       -> return Any
      R.Vector _  -> coerce $
        Vector    <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} s)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Expr      -> coerce $
        Expr      <$> (fromIntegral <$> {#get VECSEXP->vecsxp.truelength #} s)
                  <*> Vector.unsafeFromSEXP (unsafeCoerce s)
      R.Bytecode  -> return $ Bytecode
      R.ExtPtr    -> coerce $
        ExtPtr    <$> {#get SEXP->u.listsxp.carval #} s
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.cdrval #} s)
                  <*> (R.sexp <$> {#get SEXP->u.listsxp.tagval #} s)
      R.WeakRef   -> coerce $ error "peekHExp: Unimplemented."
      R.Raw       -> coerce $ error "peekHExp: Unimplemented."
      R.S4        -> coerce $ error "peekHExp: Unimplemented."
      _           -> coerce $ error "peekHExp: Unimplemented."

pokeHExp :: Ptr (HExp a) -> HExp a -> IO ()
pokeHExp s h = do
    nil <- peek LR.nilValuePtr
    let s' = castPtr s
    case h of
         Nil -> return ()
         Symbol pname value internal -> do
           {#set SEXP->u.symsxp.pname #} s' (R.unsexp pname)
           -- Set the value lazily to deal with self valued symbols.
           unsafeInterleaveIO $ {#set SEXP->u.symsxp.value #} s' (R.unsexp value)
           maybe ({#set SEXP->u.symsxp.internal#} s' (R.unsexp nil))
                 ({#set SEXP->u.symsxp.internal#} s' . R.unsexp) internal
         List carval cdrval tagval -> do
           {#set SEXP->u.listsxp.carval #} s' (R.unsexp carval)
           maybe ({#set SEXP->u.listsxp.cdrval#} s' (R.unsexp nil))
                 ({#set SEXP->u.listsxp.cdrval#} s' . R.unsexp) cdrval
           maybe ({#set SEXP->u.listsxp.tagval#} s' (R.unsexp nil))
                 ({#set SEXP->u.listsxp.tagval#} s' . R.unsexp) tagval
         Env frame enclos hashtab -> do
           {#set SEXP->u.envsxp.frame #} s' (R.unsexp frame)
           {#set SEXP->u.envsxp.enclos #} s' (R.unsexp enclos)
           {#set SEXP->u.envsxp.hashtab #} s' (R.unsexp hashtab)
         Closure formals body env -> do
           {#set SEXP->u.closxp.formals #} s' (R.unsexp formals)
           {#set SEXP->u.closxp.body #} s' (R.unsexp body)
           {#set SEXP->u.closxp.env #} s' (R.unsexp env)
         Promise value expr env -> do
           {#set SEXP->u.promsxp.value #} s' (R.unsexp value)
           {#set SEXP->u.promsxp.expr #} s' (R.unsexp expr)
           {#set SEXP->u.promsxp.env #} s' (R.unsexp env)
         Lang carval cdrval -> do
           {#set SEXP->u.listsxp.carval #} s' (R.unsexp carval)
           {#set SEXP->u.listsxp.cdrval #} s' (R.unsexp cdrval)
         Special offset -> do
           {#set SEXP->u.primsxp.offset #} s' (fromIntegral offset)
         Builtin offset -> do
           {#set SEXP->u.primsxp.offset #} s' (fromIntegral offset)
         Char _vc        -> error "pokeHExp: Unimplemented."
         Logical  _vt    -> error "pokeHExp: Unimplemented."
         Int  _vt        -> error "pokeHExp: Unimplemented."
         Real _vt        -> error "pokeHExp: Unimplemented."
         String _vt      -> error "pokeHExp: Unimplemented."
         Complex _vt     -> error "pokeHExp: Unimplemented."
         Vector _v _     -> error "pokeHExp: Unimplemented."
         Bytecode        -> error "pokeHExp: Unimplemented."
         ExtPtr _ _ _    -> error "pokeHExp: Unimplemented."
         WeakRef _ _ _ _ -> error "pokeHExp: Unimplemented."
         Raw     _       -> error "pokeHExp: Unimplemented."
         S4      _       -> error "pokeHExp: Unimplemented."
         DotDotDot _     -> error "pokeHExp: Unimplemented."
         Expr _ _        -> error "pokeHExp: Unimplemented."
         Any             -> return ()

-- | A view function projecting a view of 'SEXP' as an algebraic datatype, that
-- can be analyzed through pattern matching.
hexp :: SEXP a -> HExp a
hexp = unsafePerformIO . peek

-- | Inverse hexp view to the real structure, note that for scalar types
-- hexp will allocate new SEXP, and @unhexp . hexp@ is not an identity function.
-- however for vector types it will return original SEXP.
unhexp :: HExp a -> SEXP a
unhexp = unsafePerformIO . unhexpIO
  where
    unhexpIO :: HExp a -> IO (SEXP a)
    unhexpIO   Nil = R.allocSEXP R.Nil
    unhexpIO s@(Symbol{})  = R.allocSEXP R.Symbol >>= \x -> poke x s >> return x
    unhexpIO s@(List{})    = R.allocSEXP R.List >>= \x -> poke x s >> return x
    unhexpIO s@(Lang{})    = R.allocSEXP R.Lang >>= \x -> poke x s >> return x
    unhexpIO s@(Env{})     = R.allocSEXP R.Env >>= \x -> poke x s >> return x
    unhexpIO s@(Closure{}) = R.allocSEXP R.Closure >>= \x -> poke x s >> return x
    unhexpIO s@(Special{}) = R.allocSEXP R.Special >>= \x -> poke x s >> return x
    unhexpIO s@(Builtin{}) = R.allocSEXP R.Builtin >>= \x -> poke x s >> return x
    unhexpIO s@(Promise{}) = R.allocSEXP R.Promise >>= \x -> poke x s >> return x
    unhexpIO  (Bytecode{}) = error "unhexp: Unimplemented."
    unhexpIO Any           = R.allocSEXP R.Any
    unhexpIO (Real vt)     = return $ Vector.toSEXP vt
    unhexpIO (Logical vt)  = return $ Vector.toSEXP vt
    unhexpIO (Int vt)      = return $ Vector.toSEXP vt
    unhexpIO (Complex vt)  = return $ Vector.toSEXP vt
    unhexpIO (Vector _ vt) = return $ Vector.toSEXP vt
    unhexpIO (Char vt)     = return $ Vector.toSEXP vt
    unhexpIO (String vt)   = return $ Vector.toSEXP vt
    unhexpIO Raw{}         = error "unhexp: Unimplemented."
    unhexpIO S4{}          = error "unhexp: Unimplemented."
    unhexpIO Expr{}        = error "unhexp: Unimplemented."
    unhexpIO WeakRef{}     = error "unhexp: Unimplemented."
    unhexpIO DotDotDot{}   = error "unhexp: Unimplemented."
    unhexpIO ExtPtr{}      = error "unhexp: Unimplemented."

-- | Project the vector out of 'SEXP's.
vector :: SEXP (R.Vector a) -> Vector.Vector a
vector (hexp -> Char vec)     = vec
vector (hexp -> Logical vec)  = vec
vector (hexp -> Int vec)      = vec
vector (hexp -> Real vec)     = vec
vector (hexp -> Complex vec)  = vec
vector (hexp -> String vec)   = vec
vector (hexp -> Vector _ vec) = vec
vector _ = error "vector: Not a vector."

-- | Like 'vector', but for expression vectors, whose type index differs from
-- that of other vectors.
expression :: SEXP R.Expr -> Vector.Vector (SEXP R.Any)
expression (hexp -> Expr _ vec) = vec
expression _ = error "expr: Not an expression."

-- | Check if SEXP is nill
maybeNil :: SEXP a
         -> IO (Maybe (SEXP a))
maybeNil s = do
  nil <- peek LR.nilValuePtr
  return $
    if R.unsexp s == R.unsexp nil
      then Nothing
      else Just s

-- | Create a symbol that loops on itself.
symbolLoop :: R (SEXP R.Symbol)
symbolLoop = io $ do
  chr <- withCString "" R.mkChar
  let x = unhexp $ Symbol chr nullPtr Nothing
  x `seq` {#set SEXP->u.symsxp.value #} (R.unsexp x) (R.unsexp x)
  return x

-- | Inject object inside car field.
injectCar :: SEXP a -> SEXP b -> R ()
injectCar s cr = io $ {#set SEXP->u.listsxp.carval #} (R.unsexp s) (R.unsexp cr)

-- | Inject object inside cdr field.
injectCdr :: SEXP a -> SEXP b -> R ()
injectCdr s cr = io $ {#set SEXP->u.listsxp.cdrval #} (R.unsexp s) (R.unsexp cr)

injectTag :: SEXP a -> SEXP b -> R ()
injectTag s tg = io $ {#set SEXP->u.listsxp.tagval #} (R.unsexp s) (R.unsexp tg)
