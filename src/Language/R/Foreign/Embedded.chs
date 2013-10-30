-- |
-- Copyright: (C) 2013, Amgen, Inc.
--
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

#include <R.h>
#include <Rembedded.h>
module Language.R.Foreign.Embedded
  ( initEmbeddedR
  , endEmbeddedR
  , replDLLinit
  , replDLLdo1
  ) where

import Foreign
import Foreign.C

{# import Language.R.Foreign.Internal #}

{# fun Rf_initEmbeddedR as initEmbeddedR { `Int', castPtr `Ptr CString' } -> `()' #}
{# fun Rf_endEmbeddedR as  endEmbeddedR { `Int' } -> `()' #} -- TODO change to boolean

{# fun setup_Rmainloop as setupRmainloop {} -> `()' #}
{# fun R_ReplDLLinit as replDLLinit {} -> `()' #}

{# fun R_ReplDLLdo1 as replDLLdo1 {} -> `Int' #}
