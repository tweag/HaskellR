-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/Rembedded.h>@, containing entry points for running an
-- instance of R embedded within another program.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R.Embedded
  ( initEmbeddedR
  , endEmbeddedR
  ) where

import Foreign
import Foreign.C

#include <Rembedded.h>
#include "missing_r.h"

-- | Initialize R.
initEmbeddedR :: Int -> Ptr CString -> IO ()
initEmbeddedR (fromIntegral -> argc) argv = c_initEmbeddedR argc argv

foreign import ccall safe "Rembedded.h Rf_initEmbeddedR" c_initEmbeddedR
  :: CInt -> Ptr CString -> IO ()

endEmbeddedR :: Int -> IO ()
endEmbeddedR (fromIntegral -> retCode) = c_endEmbeddedR retCode

foreign import ccall safe "Rembedded.h Rf_endEmbeddedR" c_endEmbeddedR
  :: CInt -> IO ()
