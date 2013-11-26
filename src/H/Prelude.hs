-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude
  ( module Data.IORef
  , module Foreign
  , module H.HVal
  , module Language.R.Interpreter
  , liftR
  , symbol
  , string
  , strings
  , install
  -- * evaluation constructs
  , eval
  , evalIO
  , eval_
  -- * constants
  , unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import           H.Prelude.Constants
import           H.HExp
import           H.HVal
import qualified Data.Vector.SEXP as Vector   
import qualified Foreign.R as R
import qualified Language.R as LR

import           Control.Applicative
import           Control.Monad ( void )
import           Foreign hiding ( unsafePerformIO, void )
import           System.IO.Unsafe ( unsafePerformIO )

-- Reexported modules.
import Data.IORef
import Language.R.Interpreter

liftR :: (R.SEXP a -> b) -> R.SomeSEXP -> b
liftR f (R.SomeSEXP x) = f (castPtr x)

symbol :: String -> R.SEXP R.Symbol
symbol = unsafePerformIO . LR.symbol

install :: String -> R.SEXP R.Symbol
install = unsafePerformIO . LR.install

string :: String -> R.SEXP (R.Vector Word8)
string = unsafePerformIO . LR.string

strings :: String -> R.SEXP (R.String)
strings = unsafePerformIO . LR.strings

-- | Evaluate R expression. Purely this function
-- may be usefull inside fully pure code
eval :: R.SEXP a -> R.SEXP b
eval = unsafePerformIO . evalIO

-- | Evaluate inside IO monad
evalIO :: R.SEXP a -> IO (R.SEXP b)
evalIO (hexp -> Expr v) =
    last <$> mapM LR.eval (Vector.toList v)
evalIO x = LR.eval x

-- | Silent version of 'evalIO' function. Discards result
eval_ :: R.SEXP a -> IO ()
eval_ = void . evalIO
