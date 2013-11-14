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

-- Reexported modules.
import Data.IORef
import Foreign
import Foreign.R hiding (protect)
import H.HVal
import Language.R.Interpreter

liftR :: (R.SEXP a -> b) -> R.SomeSEXP -> b
liftR f (R.SomeSEXP x) = f (castPtr x)
