-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude.Eval
  ( evalH
  , eval_
  ) where

import           H.HExp
-- import           H.Prelude.Globals as H
import           H.Monad
import qualified Foreign.R as R
import qualified Language.R as LR
import qualified Data.Vector.SEXP as Vector

import           Control.Applicative
import           Control.Monad ( void )

-- | Evaluate expression.
evalH :: R.SEXP a -> R (R.SEXP b)
evalH = io . evalIO

-- | Evaluate inside IO monad.
evalIO :: R.SEXP a -> IO (R.SEXP b)
{-
evalIO h@(hexp -> Promise s ex rho)
    | R.unsexp s == R.unsexp H.unboundValue = do
      val <- LR.evalEnv ex rho
      injectCar h val
      injectTag h H.nilValue
      return val
    | otherwise = return (R.sexp . R.unsexp $ s)
-}
evalIO (hexp -> Expr _ v) =
    last <$> mapM LR.eval (Vector.toList v)
evalIO x = LR.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: R.SEXP a -> R ()
eval_ = void . evalH
