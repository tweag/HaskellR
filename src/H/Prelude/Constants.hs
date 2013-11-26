-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude.Constants
  ( unboundValue
  , globalEnv
  , nilValue
  , missingArg
  ) where

import Foreign.R  (SEXP, SEXPTYPE(..))
import qualified Language.R as R
import Data.IORef ( readIORef )
import System.IO.Unsafe ( unsafePerformIO )

unboundValue :: SEXP Symbol
unboundValue = unsafePerformIO $ readIORef R.unboundValue

globalEnv :: SEXP Env
globalEnv = unsafePerformIO $ readIORef R.globalEnv

nilValue :: SEXP Nil
nilValue = unsafePerformIO $ readIORef R.nilValue

missingArg :: SEXP Symbol
missingArg = unsafePerformIO $ readIORef R.missingArg
