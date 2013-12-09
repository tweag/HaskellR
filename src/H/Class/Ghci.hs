-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not intentent to be used outside of GHCI session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Class.Ghci
  ( MonadR(..)
  ) where

import Control.Monad.Reader
import Language.R.Interpreter ( runInRThread )

import H.Monad
import H.Internal.REnv

-- This class is intendent for creation and running
-- H code.
class MonadR m where
  runR :: R a -> m a

instance MonadR IO where
  runR (R x) = runInRThread $ runReaderT x REnv
