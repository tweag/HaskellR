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

import H.Constraints
import qualified Foreign.R as R
-- XXX Duplicate import to make c2hs happy. The problem is that c2hs doesn't
-- like the "as R" of the above import.
{#import Foreign.R #}

import Foreign
import Foreign.C

import Data.Word (Word8)

{#enum ParseStatus {} deriving (Eq, Show) #}

-- TODO: use ParseStatus or write a wrapper for parseVector.
{#fun R_ParseVector as parseVector
  `(In a (R.Nil :+: R.Vector Word8))'
  => { unSEXP `SEXP (R.Vector Word8)'
     , `Int'
     , id `Ptr CInt'
     , unSEXP `SEXP a' }
  -> `SEXP (R.Vector (R.SEXP R.Expr))' SEXP #}
