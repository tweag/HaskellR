-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude.Eval
  ( eval
  , evalIO
  , eval_
  ) where

import           H.HExp
import           H.Prelude.Globals as H
import qualified Foreign.R as R
import qualified Language.R as LR
import qualified Data.Vector.SEXP as Vector   

import           Control.Applicative
import           Control.Monad ( void )
import           System.IO.Unsafe ( unsafePerformIO )

-- | Evaluate R expression. Purely this function
-- may be usefull inside fully pure code
eval :: R.SEXP a -> R.SEXP b
eval = unsafePerformIO . evalIO

-- | Evaluate inside IO monad
evalIO :: R.SEXP a -> IO (R.SEXP b)
evalIO h@(hexp -> Promise s ex rho)
    | R.unsexp s == R.unsexp H.unboundValue = do
      val <- LR.evalEnv ex rho
      injectCar h val
      injectTag h H.nilValue
      return val
    | otherwise = return (R.sexp . R.unsexp $ s)
evalIO (hexp -> Expr v) =
    last <$> mapM LR.eval (Vector.toList v)
evalIO x = LR.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: R.SEXP a -> IO ()
eval_ = void . evalIO
