{-# Language DataKinds #-}

module Language.R.Internal where

import Control.Memory.Region
import Data.ByteString (ByteString)
import Foreign.R (SEXP)

r1 :: ByteString -> SEXP s -> IO (SEXP V)
r2 :: ByteString -> SEXP s -> SEXP s -> IO (SEXP V)
installIO :: String -> IO (SEXP V)
