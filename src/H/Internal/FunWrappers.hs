-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Helpers for passing functions pointers between Haskell and R
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
module H.Internal.FunWrappers
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

import           H.Internal.TH
import           Foreign ( FunPtr )
import qualified Foreign.R as R

foreign import ccall "wrapper" wrap0 :: IO (R.SEXP a) -> IO (FunPtr (IO (R.SEXP a)))

foreign import ccall "wrapper" wrap1
    :: (R.SEXP a -> IO (R.SEXP b)) -> IO (FunPtr (R.SEXP a -> IO (R.SEXP b)))

foreign import ccall "wrapper" wrap2
    :: (R.SEXP a -> R.SEXP b -> IO (R.SEXP c))
    -> IO (FunPtr (R.SEXP a -> R.SEXP b -> IO (R.SEXP c)))

foreign import ccall "wrapper" wrap3
    :: (R.SEXP a -> R.SEXP b -> R.SEXP c -> IO (R.SEXP d))
    -> IO (FunPtr (R.SEXP a -> R.SEXP b -> R.SEXP c -> IO (R.SEXP d)))

$(thWrappers 4 25)
