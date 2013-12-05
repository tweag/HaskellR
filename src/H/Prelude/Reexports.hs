-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude.Reexports
  ( symbol
  , string
  , strings
  , install
  ) where

import qualified Foreign.R as R
import qualified Language.R as LR

import           Data.Word
import           System.IO.Unsafe ( unsafePerformIO )

symbol :: String -> R.SEXP R.Symbol
symbol = unsafePerformIO . LR.symbol

install :: String -> R.SEXP R.Symbol
install = unsafePerformIO . LR.install

string :: String -> R.SEXP (R.Vector Word8)
string = unsafePerformIO . LR.string

strings :: String -> R.SEXP (R.String)
strings = unsafePerformIO . LR.strings
