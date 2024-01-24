-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}

-- Sidesteps a failure when verifying: liftIO . protect
{-@ LIQUID "--prune-unsorted" @-}

module Control.Monad.R.Class
  ( MonadR(..)
  , Region
  ) where

import Control.Memory.Region
import qualified Data.Kind
import Foreign.C -- XXX: only needed to help name resolution in LH
import Foreign.R

import Control.Applicative
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Primitive (PrimMonad, PrimState)
import Prelude

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'Language.R.Instance.R' monad. For convenience, in a GHCi session, we
-- normally use the 'IO' monad directly (by means of a 'MonadR' instance for
-- 'IO', imported only in GHCi).
class (Applicative m, MonadIO m, MonadCatch m, MonadMask m, PrimMonad m)
      => MonadR m where
  -- | Lift an 'IO' action.
  io :: IO a -> m a
  io = liftIO

  {-@ acquire :: a:SEXP V -> m (TSEXP (Region m) (typeOf a)) @-}
  -- | Acquire ownership in the current region of the given object. This means
  -- that the liveness of the object is guaranteed so long as the current region
  -- remains active (the R garbage collector will not attempt to free it).
  acquire :: SEXP V -> m (SEXP (Region m))
  default acquire :: (MonadIO m, Region m ~ G) => SEXP V -> m (SEXP (Region m))
  acquire = liftIO . protect

  -- | A reification of an R execution context, i.e. a "session".
  data ExecContext m :: Data.Kind.Type

  -- | Get the current execution context.
  getExecContext :: m (ExecContext m)

  -- | Provides no static guarantees that resources do not extrude the scope of
  -- their region. Acquired resources are not freed automatically upon exit.
  -- For internal use only.
  unsafeRunWithExecContext :: m a -> ExecContext m -> IO a

type Region m = PrimState m
