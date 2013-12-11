-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not intentent to be used outside of GHCI session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.Ghci
  ( module H.Prelude
  , module Language.R.Runtime.QQ
  )
  where

import H.Prelude hiding ( runR )
import Language.R.Runtime.QQ

instance MonadR IO where
  io = runInRThread
