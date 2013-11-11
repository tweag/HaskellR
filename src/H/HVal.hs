-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language ExistentialQuantification #-}
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

-- Instances
{-
instance Num HVal where
    fromInteger x = SEXP (mkSEXP (fromInteger x :: Double))
    a + b = SEXP (rplus  (toSEXP a) (toSEXP b))
    a - b = SEXP (rminus (toSEXP a) (toSEXP b))
    a * b = SEXP (rmult  (toSEXP a) (toSEXP b))
-}

-- | Project from HVal to R SEXP.
--
-- Note that this function is partial.
fromHVal :: HVal -> Some R.SEXP
fromHVal (SEXP x) = Some x
fromHVal _        = error "toSEXP: not an SEXP"

-- | Safe version of 'toSEXP'.
safeFromHVal :: HVal -> Maybe (Some R.SEXP)
safeFromHVal (SEXP x) = Just x
safeFromHVal _        = Nothing

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
{-
class IsSEXP a where
  mkSEXP :: a -> R.SEXP

instance IsSEXP Double where
  mkSEXP x = unsafePerformIO $ do
    v  <- R.allocVector R.Real 1
    pt <- R.real v
    pokeByteOff pt 0 x
    return v
-}
