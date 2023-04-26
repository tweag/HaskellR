-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/R_ext/Parse.h>@.

{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <Rinternals.h>
#include <R_ext/Parse.h>
module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import qualified Foreign.R as R

import Foreign
import Foreign.C

-- | The return code of a call to 'parseVector', indicating whether the parser
-- failed or succeeded.
data ParseStatus
  = PARSE_NULL
  | PARSE_OK
  | PARSE_INCOMPLETE
  | PARSE_ERROR
  | PARSE_EOF
  deriving (Eq, Show)

instance Enum ParseStatus where
  fromEnum PARSE_NULL       = #const PARSE_NULL
  fromEnum PARSE_OK         = #const PARSE_OK
  fromEnum PARSE_INCOMPLETE = #const PARSE_INCOMPLETE
  fromEnum PARSE_ERROR      = #const PARSE_ERROR
  fromEnum PARSE_EOF        = #const PARSE_EOF
  toEnum i = case i of
    (#const PARSE_NULL)       -> PARSE_NULL
    (#const PARSE_OK)         -> PARSE_OK
    (#const PARSE_INCOMPLETE) -> PARSE_INCOMPLETE
    (#const PARSE_ERROR)      -> PARSE_ERROR
    (#const PARSE_EOF)        -> PARSE_EOF
    _ -> error "ParseStatus.fromEnum: can't mach value"

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
