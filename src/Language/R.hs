-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low level R functions
module Language.R
  ( r1
  , r2
  , globalEnv
  ) where


import Control.Exception ( bracket )
import Data.IORef ( IORef, newIORef, readIORef )
import Foreign

import qualified Foreign.R as R

globalEnv :: IORef (R.SEXP R.Env)
globalEnv = unsafePerformIO $ newIORef (R.SEXP nullPtr)

-- | Call 1-arity R function by name, function will be found in runtime,
-- using global environment, no additional environment is provided to
-- function.
-- 
-- This function is done mainly for testing purposes.
r1 :: String -> R.SEXP a -> R.SEXP b
r1 fn a =
    unsafePerformIO $ R.install fn >>= \f -> do
        protect (R.lang2 f a) (\v -> do
          gl <- readIORef globalEnv
          x <- alloca $ \p -> R.tryEval v gl p
          R.protect x
          return x)

-- | Call 2-arity R function, function will be found in runtime, using
-- global environment. See 'r1' for additional comments.
r2 :: String -> R.SEXP a -> R.SEXP b -> R.SEXP c
r2 fn a b =
    unsafePerformIO $ R.install fn >>= \f ->
      protect (R.lang3 f a b) (\v -> do
        gl <- readIORef globalEnv
        x <- alloca $ \p -> R.tryEval v gl p
        R.protect x
        return x)

protect :: IO (R.SEXP a) -> (R.SEXP a -> IO b) -> IO b
protect accure = 
   bracket (accure >>= \x -> R.protect x >> return x)
           (const (R.unprotect 1))
