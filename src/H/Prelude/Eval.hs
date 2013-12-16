-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude.Eval
  ( eval
  , eval_
  ) where

import           H.HExp
import           H.Internal.Prelude
import qualified Language.R as R
import qualified Data.Vector.SEXP as Vector

import           Control.Applicative
import           Control.Monad ( void )

-- | Evaluate expression.
eval :: MonadR m => SEXP a -> m (SEXP b)
eval = io . evalIO
  where
    evalIO :: SEXP a -> IO (SEXP b)
    evalIO (hexp -> Expr _ v) =
      last <$> mapM R.eval (Vector.toList v)
    evalIO x = R.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: MonadR m => SEXP a -> m ()
eval_ = void . eval
