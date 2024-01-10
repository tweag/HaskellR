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
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language ViewPatterns #-}

-- required to not warn about IsVector usage.
{-# OPTIONS_GHC -fno-warn-redundant-constraints -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-@ LIQUID "--exact-data-cons" @-} -- needed to have LH accept specs in module HExp
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--no-totality" @-}
module Language.R.Literal
  {-
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
  ) -} where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.SEXP.Mutable as SMVector
import qualified Data.Vector.SEXP.Mutable as Mutable -- Needed to help LH name resolution
import           Foreign.C  -- Needed to help LH name resolution
import qualified Foreign.R as R
import           Foreign.R ( SEXP )
import           GHC.ForeignPtr -- Needed to help LH name resolution
import           GHC.ST -- Needed to help LH name resolution
import           Internal.Error
import           {-# SOURCE #-} Language.R.Internal (r1)
import           Language.R.Globals (nilValue)
import           Language.R.HExp
import           Language.R.Instance
import           Language.R.Internal.FunWrappers
import           Language.R.Internal.FunWrappers.TH

import Control.DeepSeq ( NFData )
import Control.Monad ( void, zipWithM_ )
import Data.Int (Int32)
import qualified Data.ByteString.Unsafe as B
import Data.Complex (Complex)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Foreign          ( FunPtr, castPtr )
import Foreign.Storable ( Storable, pokeElemOff )
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding.UTF8
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'SEXP'.
class Literal a where
    -- | Internal function for converting a literal to a 'SEXP' value. You
    -- probably want to be using 'mkSEXP' instead.
    mkSEXPIO :: a -> IO (SEXP V)
    fromSEXP :: SEXP s -> a
    -- | Like 'fromSomeSEXP', but behaves like the @as.*@ family of functions
    -- in R, by performing a best effort conversion to the target form (e.g. rounds
    -- reals to integers, etc) for atomic types.
    dynSEXP :: SEXP s -> a

    default mkSEXPIO :: Literal [a] => a -> IO (SEXP V)
    mkSEXPIO x = mkSEXPIO [x]

    default fromSEXP :: Literal [a] => SEXP s -> a
    fromSEXP (fromSEXP -> [x]) = x
    fromSEXP _ = failure "fromSEXP" "Not a singleton vector."

    default dynSEXP :: Literal [a] => SEXP s -> a
    dynSEXP (dynSEXP -> [x]) = x
    dynSEXP _ = failure "dynSEXP" "Not a singleton vector."

-- |  Create a SEXP value and protect it in current region
mkSEXP :: (Literal a, MonadR m) => a -> m (SEXP (Region m))
mkSEXP x = io (mkSEXPIO x) >>= \a -> acquire a

{-
-- | Like 'fromSomeSEXP', but behaves like the @as.*@ family of functions
-- in R, by performing a best effort conversion to the target form (e.g. rounds
-- reals to integers, etc) for atomic types.
-- TODO: add a spec to dynSEXP
dynSEXP :: forall a s ty. Literal a ty => SEXP s -> a
dynSEXP sx =
    fromSEXP $ unsafePerformIO $ case literalRType (undefined :: a) of
      R.SChar -> r1 "as.character" sx
      R.SInt -> r1 "as.integer" sx
      R.Real -> r1 "as.double" sx
      R.SComplex -> r1 "as.complex" sx
      R.Logical -> r1 "as.logical" sx
      R.Raw -> r1 "as.raw" sx
      _ -> return $ R.release sx
-}

{-# NOINLINE mkSEXPVector #-}
{-@ mkSEXPVector :: vt:VSEXPTYPE s a -> [IO a] -> TSEXP s (vstypeOf vt) @-}
mkSEXPVector :: Storable a
             => SVector.VSEXPTYPE s a
             -> [IO a]
             -> SEXP s
mkSEXPVector ty allocators = unsafePerformIO $ mkSEXPVectorIO ty allocators

{-@ assume mkSEXPVectorIO :: vt:VSEXPTYPE s a -> [IO a] -> IO (TSEXP s (vstypeOf vt)) @-}
{-@ ignore mkSEXPVectorIO @-}
mkSEXPVectorIO :: Storable a
               => SVector.VSEXPTYPE s a
               -> [IO a]
               -> IO (SEXP s)
mkSEXPVectorIO ty allocators =
    R.withProtected (R.allocVector (SVector.vstypeOf ty) $ length allocators) $ \vec -> do
      let ptr = castPtr (R.unsafeSEXPToVectorPtr vec)
      zipWithM_ (\i -> (>>= pokeElemOff ptr i)) [0..] allocators
      return vec

{-# NOINLINE mkProtectedSEXPVector #-}
{-@
mkProtectedSEXPVector :: vt:VSEXPTYPE s a -> [SEXP s] -> TSEXP s (vstypeOf vt)
@-}
mkProtectedSEXPVector :: SVector.VSEXPTYPE s a
                      -> [SEXP s]
                      -> SEXP s
mkProtectedSEXPVector ty xs = unsafePerformIO (mkProtectedSEXPVectorIO ty xs)

{-@
assume mkProtectedSEXPVectorIO :: vt:VSEXPTYPE s a -> [SEXP s] -> IO (TSEXP s (vstypeOf vt))
ignore mkProtectedSEXPVectorIO
@-}
mkProtectedSEXPVectorIO :: SVector.VSEXPTYPE s a
                        -> [SEXP s]
                        -> IO (SEXP s)
mkProtectedSEXPVectorIO ty xs = do
    mapM_ (void . R.protect) xs
    z <- R.withProtected (R.allocVector (SVector.vstypeOf ty) $ length xs) $ \vec -> do
           let ptr = castPtr (R.unsafeSEXPToVectorPtr vec)
           zipWithM_ (pokeElemOff ptr) [0..] xs
           return vec
    R.unprotect (length xs)
    return z

instance Literal [R.Logical] where
    mkSEXPIO = mkSEXPVectorIO SVector.VLogical . map return
    fromSEXP = fromSEXPLogicalList
    dynSEXP = fromSEXP . unsafePerformIO . r1 "as.logical"

{-@ ignore fromSEXPLogicalList @-}
fromSEXPLogicalList :: SEXP s -> [R.Logical]
fromSEXPLogicalList (hexp -> Logical v) = SVector.toList v
fromSEXPLogicalList _ = error "fromSEXP @[R.Logical]: expected Logical"

instance Literal [Int32] where
    mkSEXPIO = mkSEXPVectorIO SVector.VInt . map return
    fromSEXP = fromSEXPInt32List
    dynSEXP = fromSEXP . unsafePerformIO . r1 "as.integer"

{-@ ignore fromSEXPInt32List @-}
fromSEXPInt32List :: SEXP s -> [Int32]
fromSEXPInt32List (hexp -> Int v) = SVector.toList v
fromSEXPInt32List _ = error "fromSEXP @[Int32]: expected Int"

instance Literal [Double] where
    mkSEXPIO = mkSEXPVectorIO SVector.VReal . map return
    fromSEXP = fromSEXPDoubleList
    dynSEXP = fromSEXP . unsafePerformIO . r1 "as.double"

{-@ ignore fromSEXPDoubleList @-}
fromSEXPDoubleList :: SEXP s -> [Double]
fromSEXPDoubleList (hexp -> Real v) = SVector.toList v
fromSEXPDoubleList _ = error "fromSEXP @[Double]: expected Real"

instance Literal [Complex Double] where
    mkSEXPIO = mkSEXPVectorIO SVector.VComplex . map return
    fromSEXP = fromSEXPComplexList
    dynSEXP = fromSEXP . unsafePerformIO . r1 "as.complex"

{-@ ignore fromSEXPComplexList @-}
fromSEXPComplexList :: SEXP s -> [Complex Double]
fromSEXPComplexList (hexp -> Complex v) = SVector.toList v
fromSEXPComplexList _ = error "fromSEXP @[Complex Double]: expected Complex"

instance Literal [String] where
    mkSEXPIO =
        mkSEXPVectorIO SVector.VString .
        map (\str -> GHC.withCString utf8 str (R.mkCharCE R.CE_UTF8))
    fromSEXP = fromSEXPStringList
    dynSEXP = fromSEXP

{-@ ignore fromSEXPStringList @-}
fromSEXPStringList :: SEXP s -> [String]
fromSEXPStringList (hexp -> String v) =
    map (\(hexp -> Char xs) -> SVector.toString xs) (SVector.toList v)
fromSEXPStringList _ = error "fromSEXP @[String]: expected String"

instance Literal Text where
    mkSEXPIO s =
        mkSEXPVectorIO SVector.VString
          [ B.unsafeUseAsCStringLen (T.encodeUtf8 s) $
              uncurry (R.mkCharLenCE R.CE_UTF8) ]
    fromSEXP = fromSEXPText
    dynSEXP = fromSEXP

{-@ ignore fromSEXPText @-}
fromSEXPText :: SEXP s -> Text
fromSEXPText (hexp -> String v) =
    case SVector.toList v of
      [hexp -> Char x] -> SVector.unsafeWithByteString x $ \p -> do
         pure $ T.decodeUtf8 p
      _ -> failure "fromSEXP" "Not a singleton vector"
fromSEXPText _ = error "fromSEXP @Text: expected String"

-- | Create a pairlist from an association list. Result is either a pairlist or
-- @nilValue@ if the input is the null list. These are two distinct forms.
-- TODO: Add a spec to toPairList
toPairList :: MonadR m => [(String, SEXP (Region m))] -> m (SEXP (Region m))
toPairList [] = return (R.release nilValue)
toPairList ((k, v):kvs) = do
    -- No need to protect the tag because it's in the symbol table, so won't be
    -- garbage collected.
    tag <- io $ withCString k R.install
    toPairList kvs >>= \cdr ->
      (io (do
        l <- R.cons v cdr
        R.setTag l (R.unsafeRelease tag)
        return l
      )) >>= \s -> acquire s

-- | Create an association list from a pairlist. R Pairlists are nil-terminated
-- chains of nested cons cells, as in LISP.
-- TODO: Add a spec to fromPairList
{-@ ignore fromPairList @-}
fromPairList :: SEXP s -> [(String, SEXP s)]
fromPairList (hexp -> Nil) = []
fromPairList (hexp -> List car cdr (hexp -> Symbol (hexp -> Char name) _ _)) =
    (SVector.toString name, car) : fromPairList cdr
fromPairList (hexp -> List _ _ _) =
    failure "fromPairList" "Association listed expected but tag not set."
fromPairList _ =
    failure "fromPairList" "Pairlist expected where some other expression appeared."

-- Use the default definitions included in the class declaration.
instance Literal R.Logical
instance Literal Int32
instance Literal Double
instance Literal (Complex Double)

instance Literal String where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP = fromSEXPString
    dynSEXP = fromSEXP

{-@ ignore fromSEXPString @-}
fromSEXPString :: SEXP s -> String
fromSEXPString x@(hexp -> String {})
      | [h] <- fromSEXP x = h
      | otherwise = failure "fromSEXP" "Not a singleton vector."
fromSEXPString _ = error "fromSEXP @String: expected String"

instance Storable a => Literal (SVector.Vector a) where
    mkSEXPIO = return . SVector.toSEXP
    fromSEXP = fromSEXPVector
    dynSEXP = fromSEXP

{-@ ignore fromSEXPVector @-}
-- TODO: we want to check the then branch but not the else
fromSEXPVector :: Storable a => SEXP s -> SVector.Vector a
fromSEXPVector sx =
    if Mutable.isVectorType (R.typeOf sx) then SVector.fromSEXP sx
    else error "fromSEXP @(SVector.Vector a)"

instance Storable a => Literal (SMVector.MVector V a) where
    mkSEXPIO = unsafeRunRegion . SMVector.toSEXP
    fromSEXP = fromSEXPMVector
    dynSEXP = fromSEXP

{-@ ignore fromSEXPMVector @-}
-- TODO: we want to check the then branch but not the else
fromSEXPMVector :: Storable a => SEXP s -> SMVector.MVector V a
fromSEXPMVector sx =
    if Mutable.isVectorType (R.typeOf sx)
    then SMVector.fromSEXP (R.release sx)
    else error "fromSEXP @(SMVector.Vector V a)"

instance Literal (SEXP s) where
    mkSEXPIO = fmap R.unsafeRelease . return
    fromSEXP = R.unsafeRelease
    dynSEXP = fromSEXP

instance (NFData a, Literal a) => Literal (R s a) where
    mkSEXPIO = funToSEXP wrap0
    fromSEXP = unimplemented "Literal (R s a) fromSEXP"
    dynSEXP = unimplemented "Literal (R s a) dynSEXP"

instance (NFData b, Literal a, Literal b) => Literal (a -> R s b) where
    mkSEXPIO = funToSEXP wrap1
    fromSEXP = unimplemented "Literal (a -> R s b) fromSEXP"
    dynSEXP = unimplemented "Literal (a -> R s b) dynSEXP"

instance (NFData c, Literal a, Literal b, Literal c)
         => Literal (a -> b -> R s c) where
    mkSEXPIO   = funToSEXP wrap2
    fromSEXP = unimplemented "Literal (a -> b -> IO c) fromSEXP"
    dynSEXP = unimplemented "Literal (a -> b -> IO c) dynSEXP"

-- | A class for functions that can be converted to functions on SEXPs.
class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance (NFData a, Literal a) => HFunWrap (R s a) (IO R.SEXP0) where
    hFunWrap a = fmap R.unsexp $ (mkSEXPIO $!) =<< unsafeRunRegion a

instance (Literal a, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP0 -> wb) where
    hFunWrap f a = hFunWrap $ f $! fromSEXP (R.SEXP a :: SEXP s)

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr a -> IO (SEXP s)

{-@ assume funToSEXP :: (b -> IO (FunPtr b)) -> a -> IO (TSEXP s 'R.ExtPtr) @-}
funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> IO (SEXP s)
funToSEXP w x = funPtrToSEXP =<< w (hFunWrap x)

$(thWrapperLiterals 3 12)
