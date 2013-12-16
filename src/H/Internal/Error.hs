-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Wrappers around 'error' that classify problems into whether these are bugs
-- internal to H, or whether they are due to a mistake by the user.
--
-- This module should only be imported by H modules and not reexported.

module H.Internal.Error
  ( module Foreign.R.Error ) where

import Foreign.R.Error
