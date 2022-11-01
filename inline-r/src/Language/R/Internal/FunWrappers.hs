-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Helpers for passing functions pointers between Haskell and R.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.R.Internal.FunWrappers where

import Foreign.R (SEXP0(..))
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

$(thWrappers 4 12)
