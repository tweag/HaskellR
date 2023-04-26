-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/R_ext/Parse.h>@.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import Foreign.R.Type (ParseStatus(..))
import qualified Foreign.R as R

import Foreign
import Foreign.C

-- | @parseVector text num status source@ parses the input string into an AST.
-- @source@, if provided, names the origin of @text@ (e.g. a filename). @num@
-- limits the number of expressions to parse, or @-1@ if no limit.

-- TODO: use ParseStatus or write a wrapper for parseVector.
parseVector
  :: R.SEXP s
  -> Int
  -> Ptr CInt
  -> R.SEXP s
  -> IO (R.SEXP s)
parseVector (R.unsexp -> s) (fromIntegral -> cnt) reti (R.unsexp -> input) =
  R.sexp <$> c_parseVector s cnt reti input

foreign import ccall "R_ext/Parse.h R_ParseVector" c_parseVector
  :: R.SEXP0 -> CInt -> Ptr CInt -> R.SEXP0 -> IO R.SEXP0
