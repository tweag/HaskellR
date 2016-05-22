-- |
-- | Copyright: (C) 2013 Amgen, Inc.
--
-- DEPRECATED: use "Language.R" instead.

{-# LANGUAGE CPP #-}

module H.Prelude
  ( module Language.R.Instance
  , module Control.Monad.R.Class
  , module Foreign.R.Error
  -- * Language.R functions
  , module Language.R
-- Not supported on Windows.
#ifndef mingw32_HOST_OS
  , module Language.R.Event
#endif
  , module Language.R.HExp
  , module Language.R.Literal
  , module Language.R.QQ
  -- * Globals
  , module Language.R.Globals
  ) where

import           Control.Monad.R.Class
import           Language.R.HExp

-- Reexported modules.
import           Language.R hiding (SEXPTYPE(..))
#ifndef mingw32_HOST_OS
import           Language.R.Event (refresh)
#endif
import           Language.R.Globals
import           Language.R.Instance
import           Language.R.Literal
import           Language.R.QQ
import           Foreign.R.Error
