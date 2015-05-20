{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Global variables used by the R interpreter. All are constant, but the values
-- of some of them may change over time (e.g. the global environment).

module Language.R.Globals
  ( unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import Control.Memory.Region
import Foreign ( peek )
import Foreign.R  (SEXP, SEXPTYPE(..))
import qualified Foreign.R as R
import System.IO.Unsafe ( unsafePerformIO )

-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP G 'Symbol
unboundValue = unsafePerformIO $ peek R.unboundValue

-- | R's @NULL@ value.
nilValue :: SEXP G 'Nil
nilValue = unsafePerformIO $ peek R.nilValue

-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP G 'Symbol
missingArg = unsafePerformIO $ peek R.missingArg

-- | The global environment.
globalEnv :: SEXP G 'Env
globalEnv = unsafePerformIO $ peek R.globalEnv
