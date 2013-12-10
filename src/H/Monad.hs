-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Monad
  ( R
  , REnv
  , runR
  -- * Reexports
  , MonadR(..)
  ) where

import H.Internal.REnv

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Catch
import Language.R ( MonadR(..) )
import Language.R.Interpreter ( runInRThread )

newtype R a = R (ReaderT REnv IO a) deriving (Monad, MonadIO, Functor, MonadCatch, Applicative)

-- | Execute R monad.
runR :: REnv -> R a -> IO a
runR env (R (ReaderT f)) = runInRThread $ f env

instance MonadR R
