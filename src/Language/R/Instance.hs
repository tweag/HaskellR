-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Interaction with an instance of R. The interface in this module allows for
-- instantiating an arbitrary number of concurrent R sessions, even though
-- currently the R library only allows for one global instance, for forward
-- compatibility.
--
-- The 'R' monad defined here serves to give static guarantees that an instance
-- is only ever used after it has been initialized and before it is finalized.
-- Doing otherwise should result in a type error. This is done in the same way
-- that the 'Control.Monad.ST' monad encapsulates side effects: by assigning
-- a rank-2 type to the only run function for the monad.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.R.Instance
  ( -- * The R monad
    R
  , runR
  , unsafeRToIO
  , unsafeRunInRThread
  , Config(..)
  , defaultConfig
  -- * R instance creation
  , initialize
  -- * R global constants
  -- $ghci-bug
  , pokeRVariables
  , globalEnvPtr
  , baseEnvPtr
  , nilValuePtr
  , unboundValuePtr
  , missingArgPtr
  , rInteractive
  , rInputHandlersPtr
  ) where

import           Control.Monad.R.Class
import           Foreign.R.Runner

import Control.Applicative
import Control.Exception ( bracket_ )
#if MIN_VERSION_exceptions(0,6,0)
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )
#elif MIN_VERSION_exceptions(0,4,0)
import Control.Monad.Catch ( MonadCatch, MonadThrow )
#else
import Control.Monad.Catch ( MonadCatch )
#endif
import Control.Monad.Reader

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s a = R { _unR :: IO a }
#if MIN_VERSION_exceptions(0,6,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative)
#elif MIN_VERSION_exceptions(0,4,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadThrow, Applicative)
#else
  deriving (Monad, MonadIO, Functor, MonadCatch, Applicative)
#endif


instance MonadR (R s) where
  io m = R $ unsafeRunInRThread m

-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance.
runR :: Config -> (forall s. R s a) -> IO a
runR config (R m) = bracket_ (initialize config) finalize m

-- | Run an R action in the global R instance from the IO monad. This action is
-- unsafe in the sense that use of it bypasses any static guarantees provided by
-- the R monad, in particular that the R instance was indeed initialized and has
-- not yet been finalized. It is a backdoor that should not normally be used.
unsafeRToIO :: R s a -> IO a
unsafeRToIO (R m) = m
