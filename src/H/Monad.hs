-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Monad
  ( R(..)
  , REnv
  , MonadR(..)
  , io
  ) where

import H.Internal.REnv

import Control.Monad.Reader
import Control.Monad.Catch

newtype R a = R (ReaderT REnv IO a) deriving (Monad, MonadIO, Functor, MonadCatch)

-- This class is intendent for creation and running
-- H code.
class MonadR m where
  runR :: R a -> m a

io :: IO a -> R a
io = liftIO
