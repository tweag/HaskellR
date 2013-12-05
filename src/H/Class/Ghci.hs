-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not intentent to be used outside of GHCI session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Class.Ghci
  ( 
  ) where

import Control.Monad.Reader

import H.Monad
import H.Internal.REnv

instance MonadR IO where
  runR (R x) = runReaderT x REnv
