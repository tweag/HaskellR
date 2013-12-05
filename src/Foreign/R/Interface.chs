-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.R.Interface
  ( -- * stack limit functions
    rCStackLimit
  , rCStackStart
  , StackSize
  ) where

import Foreign
import Foreign.C

#define CSTACK_DEFNS
#include "Defn_pruned.h"

type StackSize = {# type uintptr_t #}

-- | Stack limit for R
foreign import ccall "&R_CStackLimit" rCStackLimit :: Ptr StackSize

-- | Start of the Stack for R
foreign import ccall "&R_CStackStart" rCStackStart :: Ptr {# type uintptr_t #}

