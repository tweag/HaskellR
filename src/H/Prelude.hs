-- |
-- Copyright: (C) 2013 Amgen, Inc.
{-# Language ViewPatterns #-}
{-# Language GADTs #-}
module H.Prelude
  ( module Data.IORef
  , module Language.R.Interpreter
  , module Foreign.R.Error
  , print
  -- * Evaluation constructs
  , module H.Prelude.Eval
  -- * R Value
  , module H.Prelude.RVal
  -- * Language.R functions
  , module H.Prelude.Reexports
  -- * Literals
  , module H.Internal.Literal
  -- * Globals
  , module H.Prelude.Globals
  , R
  , MonadR(..)
  , runR
  , withProtected
  ) where


import           H.Internal.Prelude
import qualified Foreign.R as R

-- Reexported modules.
import           H.Prelude.Reexports
import           H.Prelude.Eval
import           H.Prelude.Globals
import           H.Prelude.RVal
import           H.Internal.Literal
import Data.IORef
import Language.R.Interpreter
import Foreign.R.Error

import Control.Monad.Catch

import Prelude hiding (print)

print :: (MonadR m) => SEXP a -> m ()
print = io . R.printValue

withProtected :: (MonadR m, MonadCatch m) => m (SEXP a) -> ((SEXP a) -> m b) -> m b
withProtected accure =
    bracket (accure >>= \x -> io $ R.protect x >> return x)
            (const (io $ R.unprotect 1))
