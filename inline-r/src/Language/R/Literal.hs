-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# Language ConstraintKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language ViewPatterns #-}

module Language.R.Literal
  ( -- * Literals conversion
    Literal(..)
  , toPairList
  , fromPairList
    -- * Derived helpers
  , fromSomeSEXP
  , mkSEXP
  , dynSEXP
  , mkSEXPVector
  , mkSEXPVectorIO
  , mkProtectedSEXPVector
  , mkProtectedSEXPVectorIO
    -- * Internal
  , funToSEXP
  ) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.SEXP.Mutable as SMVector
import qualified Foreign.R as R
import           Foreign.R.Type ( IsVector, SSEXPTYPE )
import           Foreign.R ( SEXP, SomeSEXP(..) )
import           Internal.Error
import           Language.R.Internal (r1)
import           Language.R.Globals (nilValue)
import           Language.R.HExp
import           Language.R.Instance
import           Language.R.Internal.FunWrappers
import           Language.R.Internal.FunWrappers.TH

import Data.Singletons ( Sing, SingI, fromSing, sing )

import Control.Monad ( void, zipWithM_ )
import Data.Int (Int32)
import Data.Complex (Complex)
import Foreign          ( FunPtr, castPtr )
import Foreign.C.String ( withCString )
import Foreign.Storable ( Storable, pokeElemOff )
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding.UTF8
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'SEXP'.
class Literal a ty | a -> ty where
    -- | Internal function for converting a literal to a 'SEXP' value. You
    -- probably want to be using 'mkSEXP' instead.
    mkSEXPIO :: a -> IO (SEXP V ty)
    fromSEXP :: SEXP s ty -> a

    default mkSEXPIO :: (IsVector ty, Literal [a] ty) => a -> IO (SEXP V ty)
    mkSEXPIO x = mkSEXPIO [x]

    default fromSEXP :: (IsVector ty, Literal [a] ty) => SEXP s ty -> a
    fromSEXP (fromSEXP -> [x]) = x
    fromSEXP _ = failure "fromSEXP" "Not a singleton vector."

-- |  Create a SEXP value and protect it in current region
mkSEXP :: (Literal a b, MonadR m) => a -> m (SEXP (Region m) b)
mkSEXP x = acquire =<< io (mkSEXPIO x)

-- | Like 'fromSEXP', but with no static type satefy. Performs a dynamic
-- (i.e. at runtime) check instead.
fromSomeSEXP :: forall s a form. (Literal a form,SingI form) => R.SomeSEXP s -> a
fromSomeSEXP = fromSEXP . R.cast (sing :: Sing form)

-- | Like 'fromSomeSEXP', but behaves like the @as.*@ family of functions
-- in R, by performing a best effort conversion to the target form (e.g. rounds
-- reals to integers, etc) for atomic types.
dynSEXP :: forall a s ty. (Literal a ty, SingI ty) => SomeSEXP s -> a
dynSEXP (SomeSEXP sx) =
    fromSomeSEXP $ unsafePerformIO $ case fromSing (sing :: SSEXPTYPE ty) of
      R.Char -> r1 "as.character" sx
      R.Int -> r1 "as.integer" sx
      R.Real -> r1 "as.double" sx
      R.Complex -> r1 "as.complex" sx
      R.Logical -> r1 "as.logical" sx
      R.Raw -> r1 "as.raw" sx
      _ -> return $ SomeSEXP $ R.release sx

{-# NOINLINE mkSEXPVector #-}
mkSEXPVector :: (Storable (SVector.ElemRep s a), IsVector a)
             => SSEXPTYPE a
             -> [IO (SVector.ElemRep s a)]
             -> SEXP s a
mkSEXPVector ty allocators = unsafePerformIO $ mkSEXPVectorIO ty allocators

mkSEXPVectorIO :: (Storable (SVector.ElemRep s a), IsVector a)
               => SSEXPTYPE a
               -> [IO (SVector.ElemRep s a)]
               -> IO (SEXP s a)
mkSEXPVectorIO ty allocators =
    R.withProtected (R.allocVector ty $ length allocators) $ \vec -> do
      let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
      zipWithM_ (\i -> (>>= pokeElemOff ptr i)) [0..] allocators
      return vec

{-# NOINLINE mkProtectedSEXPVector #-}
mkProtectedSEXPVector :: IsVector b
                      => SSEXPTYPE b
                      -> [SEXP s a]
                      -> SEXP s b
mkProtectedSEXPVector ty xs = unsafePerformIO $ mkProtectedSEXPVectorIO ty xs

mkProtectedSEXPVectorIO :: IsVector b
                        => SSEXPTYPE b
                        -> [SEXP s a]
                        -> IO (SEXP s b)
mkProtectedSEXPVectorIO ty xs = do
    mapM_ (void . R.protect) xs
    z <- R.withProtected (R.allocVector ty $ length xs) $ \vec -> do
           let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
           zipWithM_ (pokeElemOff ptr) [0..] xs
           return vec
    R.unprotect (length xs)
    return z

instance Literal [R.Logical] 'R.Logical where
    mkSEXPIO = mkSEXPVectorIO sing . map return
    fromSEXP (hexp -> Logical v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal [Int32] 'R.Int where
    mkSEXPIO = mkSEXPVectorIO sing . map return
    fromSEXP (hexp -> Int v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal [Double] 'R.Real where
    mkSEXPIO = mkSEXPVectorIO sing . map return
    fromSEXP (hexp -> Real v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal [Complex Double] 'R.Complex where
    mkSEXPIO = mkSEXPVectorIO sing . map return
    fromSEXP (hexp -> Complex v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

instance Literal [String] 'R.String where
    mkSEXPIO =
        mkSEXPVectorIO sing .
        map (\str -> GHC.withCString utf8 str (R.mkCharCE R.CE_UTF8))
    fromSEXP (hexp -> String v) =
        map (\(hexp -> Char xs) -> SVector.toString xs) (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "String expected where some other expression appeared."

-- | Create a pairlist from an association list. Result is either a pairlist or
-- @nilValue@ if the input is the null list. These are two distinct forms. Hence
-- why the type of this function is not more precise.
toPairList :: MonadR m => [(String, SomeSEXP (Region m))] -> m (SomeSEXP (Region m))
toPairList [] = return $ SomeSEXP (R.release nilValue)
toPairList ((k, SomeSEXP v):kvs) = do
    -- No need to protect the tag because it's in the symbol table, so won't be
    -- garbage collected.
    tag <- io $ withCString k R.install
    toPairList kvs >>= \case
      SomeSEXP cdr@(hexp -> Nil) ->
        fmap SomeSEXP $ unhexp $ List v cdr (R.unsafeRelease tag)
      SomeSEXP cdr@(hexp -> List _ _ _) ->
        fmap SomeSEXP $ unhexp $ List v cdr (R.unsafeRelease tag)
      _ -> impossible "toPairList"

-- | Create an association list from a pairlist. R Pairlists are nil-terminated
-- chains of nested cons cells, as in LISP.
fromPairList :: SomeSEXP s -> [(String, SomeSEXP s)]
fromPairList (SomeSEXP (hexp -> Nil)) = []
fromPairList (SomeSEXP (hexp -> List car cdr (hexp -> Symbol (hexp -> Char name) _ _))) =
    (SVector.toString name, SomeSEXP car) : fromPairList (SomeSEXP cdr)
fromPairList (SomeSEXP (hexp -> List _ _ _)) =
    failure "fromPairList" "Association listed expected but tag not set."
fromPairList _ =
    failure "fromPairList" "Pairlist expected where some other expression appeared."

-- Use the default definitions included in the class declaration.
instance Literal R.Logical 'R.Logical
instance Literal Int32 'R.Int
instance Literal Double 'R.Real
instance Literal (Complex Double) 'R.Complex

instance Literal String 'R.String where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP x@(hexp -> String {})
      | [h] <- fromSEXP x = h
      | otherwise = failure "fromSEXP" "Not a singleton vector."
    fromSEXP _ =
        failure "fromSEXP" "String expected where some other expression appeared."

instance SVector.VECTOR V ty a => Literal (SVector.Vector V ty a) ty where
    mkSEXPIO = return . SVector.toSEXP
    fromSEXP = SVector.fromSEXP . R.cast (sing :: SSEXPTYPE ty)
             . SomeSEXP . R.release

instance SVector.VECTOR V ty a => Literal (SMVector.MVector V ty a) ty where
    mkSEXPIO v = unsafeToIO (SMVector.toSEXP v :: R V (SEXP V ty))
    fromSEXP = SMVector.fromSEXP . R.cast (sing :: SSEXPTYPE ty)
             . SomeSEXP . R.release

instance SingI a => Literal (SEXP s a) a where
    mkSEXPIO = fmap R.unsafeRelease . return
    fromSEXP = R.cast (sing :: SSEXPTYPE a) . SomeSEXP . R.unsafeRelease

instance Literal (SomeSEXP s) 'R.Any where
    -- The ANYSXP type in R plays the same role as SomeSEXP in H. It is a dummy
    -- type tag, that is never seen in any object. It serves only as a stand-in
    -- when the real type is not known.
    mkSEXPIO (SomeSEXP s) = return . R.unsafeRelease $ R.unsafeCoerce s
    fromSEXP = SomeSEXP . R.unsafeRelease

instance Literal a b => Literal (R s a) 'R.ExtPtr where
    mkSEXPIO = funToSEXP wrap0
    fromSEXP = unimplemented "Literal (R s a) fromSEXP"

instance (Literal a a0, Literal b b0) => Literal (a -> R s b) 'R.ExtPtr where
    mkSEXPIO = funToSEXP wrap1
    fromSEXP = unimplemented "Literal (a -> R s b) fromSEXP"

instance (Literal a a0, Literal b b0, Literal c c0)
         => Literal (a -> b -> R s c) 'R.ExtPtr where
    mkSEXPIO   = funToSEXP wrap2
    fromSEXP = unimplemented "Literal (a -> b -> IO c) fromSEXP"

-- | A class for functions that can be converted to functions on SEXPs.
class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (R s a) (IO R.SEXP0) where
    hFunWrap a = fmap R.unsexp $ (mkSEXPIO $!) =<< unsafeToIO a

instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP0 -> wb) where
    hFunWrap f a = hFunWrap $ f $! fromSEXP (R.sexp a :: SEXP s la)

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr a -> IO (SEXP s 'R.ExtPtr)

funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> IO (SEXP s 'R.ExtPtr)
funToSEXP w x = funPtrToSEXP =<< w (hFunWrap x)

$(thWrapperLiterals 3 12)
