{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Fib
  ( neg
  , fib
  , fact
  , factSexp
  ) where

import           H.Prelude as H hiding ( runR )
import qualified Foreign.R as R
import           Foreign.R (SEXP)
import           Data.Int (Int32)
import           Language.R.QQ


neg :: SEXP 'R.Logical
    -> SEXP 'R.Int
    -> R R.SomeSEXP -- (SEXP 'R.Int)
neg (fromSEXP -> R.True)  (fromSEXP -> n :: Int32) = [r| n_hs |]
neg (fromSEXP -> R.False) (fromSEXP -> n :: Int32) = [r| -n_hs |]
neg (fromSEXP -> R.NA)    (fromSEXP -> _ :: Int32) = [r| NA |]
neg _ _ = error "Impossible happen."

fib :: SEXP 'R.Int -> R R.SomeSEXP -- (SEXP 'R.Int)
fib (fromSEXP -> 1 :: Int32) = return $ R.SomeSEXP $ mkSEXP (1 :: Int32)
fib (fromSEXP -> 2 :: Int32) = return $ R.SomeSEXP $ mkSEXP (1 :: Int32)
fib (fromSEXP -> n :: Int32) = [r| fib_hs(as.integer(n_hs - 1)) + fib_hs(as.integer(n_hs - 2)) |]

fact :: Int32 -> R Int32
fact 0 = return 1
fact n = fmap (H.fromSEXP . R.cast R.Int) [r| as.integer(n_hs * fact_hs(as.integer(n_hs - 1))) |]

factSexp :: SEXP 'R.Int -> R (SEXP 'R.Int)
factSexp (fromSEXP -> 0 :: Int32) = return $ mkSEXP (1::Int32)
factSexp (fromSEXP -> n :: Int32) = fmap (H.fromSEXP.R.cast R.Int) [r| as.integer(n_hs * fact_hs(as.integer(n_hs -1))) |]
