-- |
-- Copyright: (C) 2013, Amgen, Inc.
--
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#include <Rinternals.h>
#include <R_ext/Parse.h>
module Math.R.Foreign.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import Foreign
import Foreign.C

{# import Math.R.Foreign.Internal #}

{# enum ParseStatus {} deriving (Eq, Show) #}

-- TODO: use ParseStatus or write a wrapper for parseVector
{# fun R_ParseVector as parseVector { id `SEXP', `Int',id `Ptr CInt', id `SEXP' } -> `SEXP' id #}
