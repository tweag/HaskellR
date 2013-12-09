-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Monad
  ( R(..)
  , REnv
  , runR
  , io
  ) where

import H.Internal.REnv

import Control.Monad.Reader
import Control.Monad.Catch
import Language.R.Interpreter ( runInRThread )

newtype R a = R (ReaderT REnv IO a) deriving (Monad, MonadIO, Functor, MonadCatch)

-- | Execute R monad.
runR :: REnv -> R a -> IO a
runR env (R (ReaderT f)) = runInRThread $ f env

io :: IO a -> R a
io = liftIO
