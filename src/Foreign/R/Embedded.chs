-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/Rembedded.h>@, containing entry points for running an
-- instance of R embedded within another program.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R.Embedded
  ( initUnlimitedEmbeddedR
  , endEmbeddedR
  ) where

import Foreign
import Foreign.C

#ifdef H_ARCH_UNIX_DARWIN
#include "darwin_c2hs_fix.h"
#endif

#include <Rembedded.h>
#include "missing_r.h"

-- | Initialize R.
{# fun H_initUnlimitedEmbeddedR as initUnlimitedEmbeddedR { `Int', castPtr `Ptr CString' } -> `()' #}

-- | Finalize R.
{# fun Rf_endEmbeddedR as  endEmbeddedR { `Int' } -> `()' #}
