-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R.Embedded
  ( initEmbeddedR
  , endEmbeddedR
  , replDLLinit
  , replDLLdo1
  ) where

import Foreign
import Foreign.C

#include <R.h>
#include <Rembedded.h>

{# fun Rf_initEmbeddedR as initEmbeddedR { `Int', castPtr `Ptr CString' } -> `()' #}
{# fun Rf_endEmbeddedR as  endEmbeddedR { `Int' } -> `()' #} -- TODO change to boolean

{# fun R_ReplDLLinit as replDLLinit {} -> `()' #}

{# fun R_ReplDLLdo1 as replDLLdo1 {} -> `Int' #}
