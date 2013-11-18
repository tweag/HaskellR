-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language ExistentialQuantification #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import qualified Language.R as R
import qualified Foreign.R  as R

import Control.Applicative
import Control.Monad ( forM_ )
import Foreign ( castPtr, newForeignPtr_, pokeElemOff )
import Foreign.C.String ( newCString )
import System.IO.Unsafe ( unsafePerformIO )

-- Temporary
import qualified Data.Vector.Storable as V
import Data.List ( intercalate )
import Data.Word

-- | Runtime universe of R Values
data HVal = forall a . SEXP (R.SEXP a)
          | HLam2 (HVal -> HVal)

instance Show HVal where
    show (SEXP s)  = unsafePerformIO $ do
      let s' = castPtr s :: R.SEXP (R.Vector Double)
      l <- R.length s'
      v <- flip V.unsafeFromForeignPtr0 l <$> (newForeignPtr_ =<< R.real s')
      return $ "[1] " ++ (intercalate " " (map show $ V.toList v))
    show (HLam2 _) = "HLam2 {..}"

-- | Project from HVal to R SEXP.
--
-- Note that this function is partial.
fromHVal :: HVal -> R.SomeSEXP
fromHVal (SEXP x) = R.SomeSEXP x
fromHVal _        = error "toSEXP: not an SEXP"

-- | Safe version of 'toSEXP'.
safeFromHVal :: HVal -> Maybe (R.SomeSEXP)
safeFromHVal (SEXP x) = Just (R.SomeSEXP x)
safeFromHVal _        = Nothing

someHVal :: R.SomeSEXP -> HVal
someHVal (R.SomeSEXP x) = SEXP x

toHVal :: R.SEXP a -> HVal
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

rplus, rminus, rmult, rfrac :: R.SEXP a -> R.SEXP a -> R.SEXP a
rplus  x y = R.r2 "+" x y
rminus x y = R.r2 "-" x y
rmult  x y = R.r2 "*" x y
rfrac  x y = R.r2 "/" x y


-- | Represents a value that can be converted into S Expression
class Literal a b | a -> b where
    mkSEXP :: a -> R.SEXP b

instance Literal Double (R.Vector Double) where
    mkSEXP x = unsafePerformIO $ do
      v  <- R.allocVector R.Real 1
      pt <- R.real v
      pokeElemOff pt 0 (fromRational . toRational $ x)
      return v

instance Literal [Double] (R.Vector Double) where
    mkSEXP x = unsafePerformIO $ do
        v  <- R.allocVector R.Real l
        pt <- R.real v
        forM_ (zip x [0..]) $ \(g,i) -> do
          pokeElemOff pt i g
        return v
      where
        l = length x

instance Literal String (R.Vector Word8) where
  mkSEXP x = unsafePerformIO $ R.mkString =<< newCString x
