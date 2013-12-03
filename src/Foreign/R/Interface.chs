-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.R.Interface
  ( -- * stack limit functions
    rCStackLimit
  , rCStackStart
  ) where

import Foreign
import Foreign.C

#define CSTACK_DEFNS
#include "Defn_pruned.h"

-- | Stack limit for R
foreign import ccall "&R_CStackLimit" rCStackLimit :: Ptr {# type uintptr_t #}

-- | Start of the Stack for R
foreign import ccall "&R_CStackStart" rCStackStart :: Ptr {# type uintptr_t #}

