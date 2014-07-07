-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- The 'R' monad defined here serves to give static guarantees that an instance
-- is only ever used after it has been initialized and before it is finalized.
-- Doing otherwise should result in a type error. This is done in the same way
-- that the 'Control.Monad.ST' monad encapsulates side effects: by assigning
-- a rank-2 type to the only run function for the monad.
--
-- This module is intended to be imported qualified.

module Control.Monad.R
  ( -- * The R monad
    R
  , runR
  , unsafeRToIO
  , unsafeRunInRThread
  , Config(..)
  , defaultConfig
  -- * R instance creation
  , initialize
  -- * Monad R
  , MonadR
  , io
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
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )
import Control.Monad.Reader

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R a = R (IO a)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative)


instance MonadR R where
  io m = R $ unsafeRunInRThread m

-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance.
runR :: Config -> R a -> IO a
runR config (R m) = bracket_ (initialize config) finalize m

-- | Run an R action in the global R instance from the IO monad. This action is
-- unsafe in the sense that use of it bypasses any static guarantees provided by
-- the R monad, in particular that the R instance was indeed initialized and has
-- not yet been finalized. It is a backdoor that should not normally be used.
unsafeRToIO :: R a -> IO a
unsafeRToIO (R m) = m
