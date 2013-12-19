-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.R.Error
  ( RError(..)
  ) where

import Control.Exception
import Data.Typeable

data RError = RError String
      deriving ( Typeable )

instance Show RError where
  show (RError s)      = "R Runtime Error: " ++ s

instance Exception RError

