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
  , eval
  -- * constants
  , unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import           H.Prelude.Constants
import           H.HExp
import qualified Foreign.R as R
import qualified Language.R as LR
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Vector.SEXP as Vector   

-- Reexported modules.
import Data.IORef
import Data.Word
import Foreign hiding ( unsafePerformIO )
import H.HVal
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

-- | Evaluate R expression
eval :: R.SEXP a -> R.SEXP b
eval (hexp -> Expr v) = last . unsafePerformIO $
    mapM LR.eval (Vector.toList v)
eval x = unsafePerformIO (LR.eval x)

