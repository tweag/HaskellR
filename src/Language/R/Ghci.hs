-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not intentent to be used outside of GHCI session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.Ghci
  where

import Language.R ( MonadR(..) )
import Language.R.Interpreter ( runInRThread )

instance MonadR IO where
  io = runInRThread
