-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings to a small subset of @<R/Rinterface.h>@, an internal R header file
-- only meant to "provide hooks for alternative front-ends, e.g. GUIs such as
-- GNOME and Cocoa".

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

-- | Stack limit for R.
foreign import ccall "&R_CStackLimit" rCStackLimit :: Ptr StackSize

-- | Start of the Stack for R.
foreign import ccall "&R_CStackStart" rCStackStart :: Ptr {# type uintptr_t #}
