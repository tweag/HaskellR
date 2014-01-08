-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/Rembedded.h>@, containing entry points for running an
-- instance of R embedded within another program.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R.Embedded
  ( initEmbeddedR
  , endEmbeddedR
  ) where

import Foreign
import Foreign.C

#include <R.h>
#include <Rembedded.h>

-- | Initialize R.
{# fun Rf_initEmbeddedR as initEmbeddedR { `Int', castPtr `Ptr CString' } -> `()' #}

-- | Finalize R.
{# fun Rf_endEmbeddedR as  endEmbeddedR { `Int' } -> `()' #} -- TODO change to boolean
