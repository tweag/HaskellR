-- |
-- Copyright: (C) 2016 Tweag I/O Limited.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.R.Internal where

import Control.Memory.Region
import Control.Monad.R.Class
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies, reify)
import Foreign.R (SEXP)

newtype AcquireIO s = AcquireIO (forall ty. SEXP V ty -> IO (SEXP s ty))

withAcquire
  :: forall m r.
     (MonadR m)
  => (forall s. Reifies s (AcquireIO (Region m)) => Proxy s -> m r)
  -> m r
withAcquire f = reify (AcquireIO (unsafeToIO . macquire)) f
  where
    macquire :: SEXP V a -> m (SEXP (Region m) a)
    macquire = acquire
