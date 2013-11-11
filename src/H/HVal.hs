-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language ExistentialQuantification #-}
{-# LANGUAGE PolyKinds  #-}
module H.HVal
  ( HVal
  -- * Conversion
  , fromHVal
  , safeFromHVal
  ) where

import qualified H.HExp as HExp
import qualified Data.Vector.Storable as S
import           Data.Some
import qualified Language.R as R
import qualified Foreign.R  as R

import Foreign

-- | Runtime universe of R Values
data HVal = forall a . SEXP (R.SEXP a)
          | HLam2 (HVal -> HVal)

--instance Show HVal where -- error
--    show (SEXP x) = "SEXP: " ++ show (unsafePerformIO (HExp.view x))

-- | Project from HVal to R SEXP.
--
-- Note that this function is partial.
fromHVal :: HVal -> Some R.SEXP
fromHVal (SEXP x) = Some x
fromHVal _        = error "toSEXP: not an SEXP"
 
-- | Safe version of 'toSEXP'.
safeFromHVal :: HVal -> Maybe (Some R.SEXP)
safeFromHVal (SEXP x) = Just (Some x)
safeFromHVal _        = Nothing

someHVal :: Some R.SEXP -> HVal
someHVal (Some x) = SEXP x
--------------------------------------------------------------------------------
-- Arithmetic subset of H                                                     --
--------------------------------------------------------------------------------
instance Num HVal where
    fromInteger x = someHVal (mkSEXP (fromInteger x :: Double))
--    a + b = SEXP (rplus  (toSEXP a) (toSEXP b))
--    a - b = SEXP (rminus (toSEXP a) (toSEXP b))
--    a * b = SEXP (rmult  (toSEXP a) (toSEXP b))

rplus, rminus, rmult, rfrac :: R.SEXP a -> R.SEXP b -> R.SEXP c
rplus  = undefined
rminus = undefined
rmult  = undefined
rfrac  = undefined
{-
-- | Arithmetics subset of H
rplus :: R.SEXP -> R.SEXP -> R.SEXP
rplus = R.r2 "+"

rminus :: R.SEXP -> R.SEXP -> R.SEXP
rminus = R.r2 "-"

rmult :: R.SEXP -> R.SEXP -> R.SEXP
rmult = R.r2 "*"

rfrac :: R.SEXP -> R.SEXP -> R.SEXP
rfrac = R.r2 "/"
-}

-- | Represents a value that can be converted into S Expression
class IsSEXP a where
  mkSEXP :: a -> Some R.SEXP

instance IsSEXP Double where
  mkSEXP x = Some $ unsafePerformIO $ do
    v  <- R.allocVector R.Real 1
    pt <- R.real v
    pokeByteOff pt 0 x
    return v
