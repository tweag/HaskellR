{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Fib
  ( neg
  , fib
  , fact
  , factSexp
  ) where

import           H.Prelude as H
import           Foreign.R.Type as R
import qualified Foreign.R as R
import           Foreign.R (SEXP)
import           Data.Int (Int32)
import           Language.R.QQ
import           Control.Applicative

neg :: SEXP s 'R.Logical
    -> SEXP s 'R.Int
    -> R s (R.SomeSEXP s)
neg (fromSEXP -> R.TRUE)  (fromSEXP -> n :: Int32) = [r| n_hs |]
neg (fromSEXP -> R.FALSE) (fromSEXP -> n :: Int32) = [r| -n_hs |]
neg (fromSEXP -> R.NA)    (fromSEXP -> _ :: Int32) = [r| NA |]
neg _ _ = error "Impossible."

fib :: SEXP s 'R.Int -> R s (R.SomeSEXP s)
fib (fromSEXP -> 1 :: Int32) = R.SomeSEXP <$> mkSEXP (1 :: Int32)
fib (fromSEXP -> 2 :: Int32) = R.SomeSEXP <$> mkSEXP (1 :: Int32)
fib (fromSEXP -> n :: Int32) = [r| fib_hs(as.integer(n_hs - 1)) + fib_hs(as.integer(n_hs - 2)) |]

fact :: Int32 -> R s Int32
fact 0 = return 1
fact n = fmap (H.fromSEXP . R.cast R.SInt) [r| as.integer(n_hs * fact_hs(as.integer(n_hs - 1))) |]

factSexp :: SEXP s 'R.Int -> R s (SEXP s 'R.Int)
factSexp (fromSEXP -> 0 :: Int32) = mkSEXP (1::Int32)
factSexp (fromSEXP -> n :: Int32) = fmap (H.fromSEXP.R.cast R.SInt) [r| as.integer(n_hs * fact_hs(as.integer(n_hs -1))) |]
