-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude.Reexports
  ( string
  , strings
  , install
  ) where

import Control.Monad.R.Class
import           H.Internal.Prelude
import qualified Foreign.R as R
import qualified Language.R as LR

install :: MonadR m => String -> m (SEXP R.Symbol)
install = io . LR.install

string :: MonadR m => String -> m (SEXP R.Char)
string = io . LR.string

strings :: MonadR m => String -> m (SEXP R.String)
strings = io . LR.strings
