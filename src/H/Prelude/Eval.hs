-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude.Eval
  ( evalH
  , eval_
  ) where

import           H.HExp
import           H.Monad
import qualified Foreign.R as R
import           Language.R ( MonadR )
import qualified Language.R as LR
import qualified Data.Vector.SEXP as Vector

import           Control.Applicative
import           Control.Monad ( void )
import           Control.Monad.IO.Class ( liftIO )

-- | Evaluate expression.
evalH :: (MonadR m) => R.SEXP a -> m (R.SEXP b)
evalH = io . evalIO

-- | Evaluate inside IO monad.
evalIO :: R.SEXP a -> IO (R.SEXP b)
evalIO (hexp -> Expr _ v) =
    last <$> mapM LR.eval (Vector.toList v)
evalIO x = LR.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: (MonadR m, Functor m) => R.SEXP a -> m ()
eval_ = void . evalH
