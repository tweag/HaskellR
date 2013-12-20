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
import qualified Foreign.R
import qualified Data.Vector.SEXP as Vector

import           Control.Applicative
import           Control.Monad ( void )

-- | Evaluate expression.
eval :: MonadR m => SEXP a -> m SomeSEXP
eval = io . evalIO
  where
    evalIO :: SEXP a -> IO SomeSEXP
    evalIO (hexp -> Expr _ v) = do
      mapM_ (\(SomeSEXP s) -> void $ Foreign.R.protect s) (Vector.toList v)
      x <- last <$> mapM (\(SomeSEXP s) -> R.eval s) (Vector.toList v)
      Foreign.R.unprotect (Vector.length v)
      return x
    evalIO x = R.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: MonadR m => SEXP a -> m ()
eval_ = void . eval
