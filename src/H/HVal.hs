-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE UndecidableInstances #-}
module H.HVal
  ( HVal
  , Literal(..)
  -- * Conversion
  , fromHVal
  , safeFromHVal
  , someHVal
  , toHVal
  -- * Arithmetics
  , rplus
  , rminus
  , rmult
  , rfrac
  ) where

import H.Internal.Prelude
import H.Internal.Literal
import qualified Language.R as R
import qualified Foreign.R  as R

import Control.Applicative
import Foreign ( castPtr , newForeignPtr_ )
import System.IO.Unsafe ( unsafePerformIO )

-- Temporary
import qualified Data.Vector.Storable as V
import Data.List ( intercalate )

-- | Runtime universe of R Values
data HVal = forall a . SEXP (SEXP a)
          | HLam2 (HVal -> HVal)

instance Show HVal where
    show (SEXP s)  = unsafePerformIO $ do
      let s' = castPtr s :: SEXP (R.Vector Double)
      l <- R.length s'
      v <- flip V.unsafeFromForeignPtr0 l <$> (newForeignPtr_ =<< R.real s')
      return $ "[1] " ++ (intercalate " " (map show $ V.toList v))
    show (HLam2 _) = "HLam2 {..}"

-- | Project from HVal to R SEXP.
--
-- Note that this function is partial.
fromHVal :: HVal -> R.SomeSEXP
fromHVal (SEXP x) = R.SomeSEXP x
fromHVal _        = failure "toSEXP" "Not a SEXP."

-- | Safe version of 'toSEXP'.
safeFromHVal :: HVal -> Maybe (R.SomeSEXP)
safeFromHVal (SEXP x) = Just (R.SomeSEXP x)
safeFromHVal _        = Nothing

someHVal :: R.SomeSEXP -> HVal
someHVal (R.SomeSEXP x) = SEXP x

toHVal :: SEXP a -> HVal
toHVal x = SEXP x

--------------------------------------------------------------------------------
-- Arithmetic subset of H                                                     --
--------------------------------------------------------------------------------
{-
instance Num HVal where
    fromInteger x = SEXP $ mkSEXP (fromInteger x :: Double)
    a + b = someHVal (rplus  (fromHVal a) (fromHVal b))
    a - b = someHVal (rminus (fromHVal a) (fromHVal b))
    a * b = someHVal (rmult  (fromHVal a) (fromHVal b))
    abs _ = error "unimplemented."
    signum _ = error "unimplemented."

instance Fractional HVal where
    fromRational x = SEXP (mkSEXP (fromRational x :: Double))
    a / b = SEXP (rfrac (fromHVal a) (fromHVal b))
-}

rplus, rminus, rmult, rfrac :: SEXP a -> SEXP a -> SEXP a
rplus  x y = R.r2 "+" x y
rminus x y = R.r2 "-" x y
rmult  x y = R.r2 "*" x y
rfrac  x y = R.r2 "/" x y
