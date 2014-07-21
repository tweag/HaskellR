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

import Foreign ( peek )
import Foreign.R  (SEXP, SEXPTYPE(..))
import qualified Foreign.R as R
import System.IO.Unsafe ( unsafePerformIO )

-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP Symbol
unboundValue = unsafePerformIO $ peek R.unboundValue

-- | R's @NULL@ value.
nilValue :: SEXP Nil
nilValue = unsafePerformIO $ peek R.nilValue

-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP Symbol
missingArg = unsafePerformIO $ peek R.missingArg

-- | The global environment.
globalEnv :: SEXP Env
globalEnv = unsafePerformIO $ peek R.globalEnv
