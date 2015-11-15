-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/Rembedded.h>@, containing entry points for running an
-- instance of R embedded within another program.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R.Embedded
  ( initEmbeddedR
  , endEmbeddedR
  ) where

import Foreign
import Foreign.C

#include <Rembedded.h>
#include "missing_r.h"

-- | Initialize R.
{# fun Rf_initEmbeddedR as initEmbeddedR { `Int', castPtr `Ptr CString' } -> `()' #}

-- | Finalize R.
{# fun Rf_endEmbeddedR as endEmbeddedR { `Int' } -> `()' #}
