-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Exception type wrapping errors thrown by the R runtime.

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
