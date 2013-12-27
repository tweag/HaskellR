-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Global variables used by the R interpreter. They are all constant, but not
-- referentially transparent (e.g. the global environment varies over time).

module Language.R.Globals
  ( unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import Foreign ( peek )
import Foreign.R  (SEXP, SEXPTYPE(..))
import qualified Language.R as R
import System.IO.Unsafe ( unsafePerformIO )

unboundValue :: SEXP Symbol
unboundValue = unsafePerformIO $ peek R.unboundValuePtr

globalEnv :: SEXP Env
globalEnv = unsafePerformIO $ peek R.globalEnvPtr

nilValue :: SEXP Nil
nilValue = unsafePerformIO $ peek R.nilValuePtr

missingArg :: SEXP Symbol
missingArg = unsafePerformIO $ peek R.missingArgPtr
