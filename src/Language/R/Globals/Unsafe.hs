-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Global variables used by the R interpreter. All are constant, but the values
-- of some of them may change over time (e.g. the global environment).

module Language.R.Globals.Unsafe
  ( unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import Foreign ( peek )
import Foreign.R.Internal  (SEXP, SEXPTYPE(..))
import qualified Foreign.R.Runner as R
import System.IO.Unsafe ( unsafePerformIO )

-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP Symbol
unboundValue = unsafePerformIO $ peek R.unboundValuePtr

-- | R's @NULL@ value.
nilValue :: SEXP Nil
nilValue = unsafePerformIO $ peek R.nilValuePtr

-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP Symbol
missingArg = unsafePerformIO $ peek R.missingArgPtr

-- | The global environment.
globalEnv :: SEXP Env
globalEnv = unsafePerformIO $ peek R.globalEnvPtr
