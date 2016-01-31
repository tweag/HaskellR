-- |
-- | Copyright: (C) 2013 Amgen, Inc.
--
-- DEPRECATED: use "Language.R" instead.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# Language GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language ViewPatterns #-}

module H.Prelude
  ( module Language.R.Instance
  , module Control.Monad.R.Class
  , module Foreign.R.Error
  -- * Language.R functions
  , module Language.R
  , module Language.R.Event
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
import           Language.R.Event (refresh)
import           Language.R.Globals
import           Language.R.Instance
import           Language.R.Literal
import           Language.R.QQ
import           Foreign.R.Error
