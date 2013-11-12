-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude
  ( module Data.IORef
  , module Foreign
  , module Foreign.R
  , module H.HVal
  , module Language.R.Interpreter
  , liftR
  ) where

import qualified Foreign.R as R
import Data.Some

-- Reexported modules.
import Data.IORef
import Foreign
import Foreign.R hiding (protect)
import H.HVal
import Language.R.Interpreter

liftR :: (R.SEXP a -> b) -> Some R.SEXP -> b
liftR f (Some x) = f (R.SEXP . R.unSEXP $ x)
