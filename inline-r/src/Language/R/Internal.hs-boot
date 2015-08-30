{-# Language DataKinds #-}

module Language.R.Internal where

import Control.Memory.Region
import Data.ByteString (ByteString)
import Foreign.R (SEXP, SomeSEXP(..))
import qualified Foreign.R.Type as R

r1 :: ByteString -> SEXP s a -> IO (SomeSEXP V)
r2 :: ByteString -> SEXP s a -> SEXP s b -> IO (SomeSEXP V)
installIO :: String -> IO (SEXP V 'R.Symbol)
