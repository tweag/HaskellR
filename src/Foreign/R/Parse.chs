-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include <Rinternals.h>
#include <R_ext/Parse.h>
module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import Foreign
import Foreign.C

{# import Foreign.R #}

{# enum ParseStatus {} deriving (Eq, Show) #}

-- TODO: use ParseStatus or write a wrapper for parseVector.
{# fun R_ParseVector as parseVector { id `SEXP', `Int', id `Ptr CInt', id `SEXP' } -> `SEXP' id #}
