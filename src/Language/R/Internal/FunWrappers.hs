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

import           H.Internal.Prelude
import           Language.R.Internal.FunWrappers.TH
import           Foreign ( FunPtr )

foreign import ccall "wrapper" wrap0 :: IO (SEXP a) -> IO (FunPtr (IO (SEXP a)))

foreign import ccall "wrapper" wrap1
    :: (SEXP a -> IO (SEXP b)) -> IO (FunPtr (SEXP a -> IO (SEXP b)))

foreign import ccall "wrapper" wrap2
    :: (SEXP a -> SEXP b -> IO (SEXP c))
    -> IO (FunPtr (SEXP a -> SEXP b -> IO (SEXP c)))

foreign import ccall "wrapper" wrap3
    :: (SEXP a -> SEXP b -> SEXP c -> IO (SEXP d))
    -> IO (FunPtr (SEXP a -> SEXP b -> SEXP c -> IO (SEXP d)))

$(thWrappers 4 25)
