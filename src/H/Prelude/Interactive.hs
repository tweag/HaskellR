-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Prelude.Interactive
  ( module H.Prelude
  , module Language.R.Runtime.QQ
  )
  where

import H.Internal.Prelude hiding (runR)
import H.Prelude hiding (runR)
import Language.R.Runtime.QQ

instance MonadR IO where
  io = runInRThread
