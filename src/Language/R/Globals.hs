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

import qualified Language.R.Globals.Unsafe as Unsafe
import           Foreign.R (SEXP, SEXPTYPE(..))
import qualified Foreign.R as R

-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP s Symbol
unboundValue = R.SEXP Unsafe.unboundValue 

-- | R's @NULL@ value.
nilValue :: SEXP s Nil
nilValue = R.SEXP Unsafe.nilValue

-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP s Symbol
missingArg = R.SEXP Unsafe.missingArg

-- | The global environment.
globalEnv :: SEXP s Env
globalEnv = R.SEXP Unsafe.globalEnv
