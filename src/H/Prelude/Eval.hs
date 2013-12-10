-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude.Eval
  ( eval
  , eval_
  ) where

import           H.HExp
import           H.Monad
import qualified Foreign.R as R
import qualified Language.R as LR
import qualified Data.Vector.SEXP as Vector

import           Control.Applicative
import           Control.Monad ( void )

-- | Evaluate expression.
eval :: MonadR m => R.SEXP a -> m (R.SEXP b)
eval = io . evalIO
  where
    evalIO :: R.SEXP a -> IO (R.SEXP b)
    evalIO (hexp -> Expr _ v) =
      last <$> mapM LR.eval (Vector.toList v)
    evalIO x = LR.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: MonadR m => R.SEXP a -> m ()
eval_ = void . eval
