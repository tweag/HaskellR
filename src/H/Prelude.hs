-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude
  ( module Data.IORef
  , module Foreign
  , module H.HVal
  , module Language.R.Interpreter
  , liftR
  -- * constants
  , unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import qualified Foreign.R as R
import qualified Language.R as LR
import System.IO.Unsafe ( unsafePerformIO )

-- Reexported modules.
import Data.IORef
import Foreign hiding ( unsafePerformIO )
import H.HVal
import Language.R.Interpreter

liftR :: (R.SEXP a -> b) -> R.SomeSEXP -> b
liftR f (R.SomeSEXP x) = f (castPtr x)

unboundValue :: R.SEXP R.Symbol
unboundValue = unsafePerformIO $ readIORef LR.unboundValue

globalEnv :: R.SEXP R.Env
globalEnv = unsafePerformIO $ readIORef LR.globalEnv

nilValue :: R.SEXP R.Nil
nilValue = unsafePerformIO $ readIORef LR.nilValue

missingArg :: R.SEXP R.Symbol
missingArg = unsafePerformIO $ readIORef LR.missingArg
