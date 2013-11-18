-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low level R functions
module Language.R
  ( r1
  , r2
  , globalEnv
  , parseFile
  ) where


import Control.Exception ( bracket )
import Data.ByteString as B
import Data.IORef ( IORef, newIORef, readIORef )
import Foreign ( alloca, nullPtr, peek )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Foreign.R as R
import qualified Foreign.R.Parse as R

globalEnv :: IORef (R.SEXP R.Env)
globalEnv = unsafePerformIO $ newIORef nullPtr

-- | Call 1-arity R function by name, function will be found in runtime,
-- using global environment, no additional environment is provided to
-- function.
--
-- This function is done mainly for testing purposes, and execution of R
-- code in case that we can't construct symbol by other methods.
r1 :: ByteString -> R.SEXP a -> R.SEXP b
r1 fn a =
    unsafePerformIO $ 
      useAsCString fn $ \cfn -> R.install cfn >>= \f -> do
        withProtected (R.lang2 f a) (\v -> do
          gl <- readIORef globalEnv
          x <- alloca $ \p -> R.tryEval v gl p
          _ <- R.protect x
          return x)

-- | Call 2-arity R function, function will be found in runtime, using
-- global environment. See 'r1' for additional comments.
r2 :: ByteString -> R.SEXP a -> R.SEXP b -> R.SEXP c
r2 fn a b =
    unsafePerformIO $ 
      useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang3 f a b) (\v -> do
        gl <- readIORef globalEnv
        x <- alloca $ \p -> R.tryEval v gl p
        _ <- R.protect x
        return x)

-- | Perform an action with resource while protecting it from the garbage
-- collection.
withProtected :: IO (R.SEXP a)      -- Action to accure resource
              -> (R.SEXP a -> IO b) -- Action
              -> IO b
withProtected accure =
   bracket (accure >>= \x -> R.protect x >> return x)
           (const (R.unprotect 1))

-- | Parse file and perform some actions on parsed file.
--
-- This function uses continuation because this is an easy way to make
-- operations GC-safe
--
-- This function is not safe to use inside GHCi.
parseFile :: FilePath -> (R.SEXP (R.Vector (R.SEXP R.Any)) -> IO a) -> IO a
parseFile fl f = do
    str <- B.readFile fl
    useAsCString str $ \cstr ->
      withProtected (R.mkString cstr) $ \rstr -> do
        rNil <- peek R.nilValue
        alloca $ \status ->
          withProtected (R.parseVector rstr (-1) status rNil) f
