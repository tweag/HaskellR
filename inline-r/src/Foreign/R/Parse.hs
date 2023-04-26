-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/R_ext/Parse.h>@.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}

module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import Control.Memory.Region -- only needed to help name resolution in LH
import Foreign.R.Type (ParseStatus(..))
import qualified Foreign.R as R

import Foreign
import Foreign.C

_ = undefined :: Control.Memory.Region.V

{-@
assume parseVector
  :: TSEXP s Foreign.R.Type.SString
  -> Int
  -> Ptr CInt
  -> {b:R.SEXP s | typeOf b == Nil || typeOf b == Foreign.R.Type.SString}
  -> IO (TSEXP s Foreign.R.Type.Expr)
@-}

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
