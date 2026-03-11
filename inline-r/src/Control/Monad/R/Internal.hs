-- |
-- Copyright: (C) 2016 Tweag I/O Limited.

{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.R.Internal where

import Control.Memory.Region
import Control.Monad.R.Class
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies, reify)
import Foreign.R (SEXP)

{-@ type AcquireIO s = forall <p :: SEXP s -> Bool > . (SEXP V)<p> -> IO ((SEXP s)<p>) @-}
type AcquireIO s = SEXP V -> IO (SEXP s)

withAcquire
  :: forall m r.
     (MonadR m)
  => (forall s. Reifies s (AcquireIO (Region m)) => Proxy s -> m r)
  -> m r
withAcquire f = do
    cxt <- getExecContext
    reify (\sx -> unsafeRunWithExecContext (acquire sx) cxt) f

getAcquireIO
  :: MonadR m => m (AcquireIO (Region m))
getAcquireIO = do
    cxt <- getExecContext
    return (\sx -> unsafeRunWithExecContext (acquire sx) cxt)
