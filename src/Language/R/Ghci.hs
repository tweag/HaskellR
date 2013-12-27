-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not intentent to be used outside of GHCI session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.R.Interactive
  ( module H.Prelude
  , module Language.R.Runtime.QQ
  , runR
  )
  where

import H.Prelude hiding ( runR )
import Language.R.Runtime.QQ
import qualified Control.Monad.R ( runR )
import H.Internal.REnv ( REnv(..) )


instance MonadR IO where
  io = runInRThread

-- | Execute R monad.
runR :: R a -> IO a
runR = Control.Monad.R.runR REnv
