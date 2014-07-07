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

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Foreign.R.Runner
import           Control.Monad.R
import           Control.Monad.R.Class

instance MonadR (R s) where
  io m = unsafeIOToR $ unsafeRunInRThread m


