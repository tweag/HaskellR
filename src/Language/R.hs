-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low level R functions
module Language.R
  ( r1
  , r2
  ) where


import Control.Exception ( bracket )
import Foreign

import qualified Foreign.R as R

-- | Call 1-arity R function by name, function will be found in runtime,
-- using global environment, no additional environment is provided to
-- function.
-- 
-- This function is done mainly for testing purposes.
r1 :: String -> R.SEXP a -> R.SEXP b
r1 fn a =
    unsafePerformIO $ R.install fn >>= \f -> do
        protect (R.lang2 f a) tryEvalZ

-- | Call 2-arity R function, function will be found in runtime, using
-- global environment. See 'r1' for additional comments.
r2 :: String -> R.SEXP a -> R.SEXP b -> R.SEXP c
r2 fn a b =
    unsafePerformIO $ R.install fn >>= \f ->
      protect (R.lang3 f a b) (\v -> do
              x <- tryEvalZ v
              R.protect x
              return x)

tryEvalZ :: R.SEXP b -> IO (R.SEXP c)
tryEvalZ x = alloca $ \p -> peek R.globalEnv >>= \e -> R.tryEval x (R.SEXP e) p

protect :: IO (R.SEXP a) -> (R.SEXP a -> IO b) -> IO b
protect accure = 
   bracket (accure >>= \x -> R.protect x >> return x)
           (const (R.unprotect 1))
