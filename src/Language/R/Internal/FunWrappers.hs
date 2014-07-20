-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Helpers for passing functions pointers between Haskell and R.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.R.Internal.FunWrappers
  ( wrap0
  , wrap1
  , wrap2
  , wrap3
  , wrap4
  , wrap5
  , wrap6
  , wrap7
  , wrap8
  , wrap9
  , wrap10
  , wrap11
  , wrap12
  , wrap13
  , wrap14
  , wrap15
  , wrap16
  , wrap17
  , wrap18
  , wrap19
  , wrap20
  , wrap21
  , wrap22
  , wrap23
  , wrap24
  , wrap25
  ) where

import Foreign.R (SEXP0)
import Language.R.Internal.FunWrappers.TH
import Foreign ( FunPtr )

foreign import ccall "wrapper" wrap0 :: IO SEXP0 -> IO (FunPtr (IO SEXP0))

foreign import ccall "wrapper" wrap1
    :: (SEXP0 -> IO SEXP0) -> IO (FunPtr (SEXP0 -> IO SEXP0))

foreign import ccall "wrapper" wrap2
    :: (SEXP0 -> SEXP0 -> IO SEXP0)
    -> IO (FunPtr (SEXP0 -> SEXP0 -> IO SEXP0))

foreign import ccall "wrapper" wrap3
    :: (SEXP0 -> SEXP0 -> SEXP0 -> IO SEXP0)
    -> IO (FunPtr (SEXP0 -> SEXP0 -> SEXP0 -> IO SEXP0))

$(thWrappers 4 25)
