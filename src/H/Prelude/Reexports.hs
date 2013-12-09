-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude.Reexports
  ( symbol
  , string
  , strings
  , install
  ) where

import           H.Monad
import qualified Foreign.R as R
import qualified Language.R as LR

import           Data.Word

symbol :: String -> R (R.SEXP R.Symbol)
symbol = io . LR.symbol

install :: String -> R (R.SEXP R.Symbol)
install = io . LR.install

string :: String -> R (R.SEXP (R.Vector Word8))
string = io . LR.string

strings :: String -> R (R.SEXP (R.String))
strings = io . LR.strings
