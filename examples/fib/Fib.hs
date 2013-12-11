{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Fib
  where

import H.Prelude as H hiding ( runR )
import qualified Foreign.R as R
import Foreign.R (SEXP)
import Data.Int
import Language.R.QQ


neg :: SEXP (R.Vector R.Logical)
       -> SEXP (R.Vector Int32)
       -> R (SEXP (R.Vector Int32))
neg (fromSEXP -> R.True)  (fromSEXP -> n :: Int32) = [r| n_hs |]
neg (fromSEXP -> R.False) (fromSEXP -> n :: Int32) = [r| -n_hs |]
neg (fromSEXP -> R.NA)    (fromSEXP -> _ :: Int32) = [r| NA |]
neg _ _ = error "Impossible happen."

fib :: SEXP (R.Vector Int32) -> R (SEXP (R.Vector Int32))
fib (fromSEXP -> 1 :: Int32) = return $ mkSEXP (1 :: Int32)
fib (fromSEXP -> 2 :: Int32) = return $ mkSEXP (1 :: Int32)
fib (fromSEXP -> n :: Int32) = [r| fib_hs(n_hs - 1) + fib_hs(n_hs - 2) |]

fact :: Int32 -> R Int32
fact 0 = return 1
fact n = fmap H.fromSEXP [r| n_hs * fact_hs(n_hs -1) |]

factSexp :: SEXP (R.Vector Int32) -> R (SEXP (R.Vector Int32))
factSexp (fromSEXP -> 0 :: Int32) = return $ mkSEXP (1::Int32)
factSexp (fromSEXP -> n :: Int32) = fmap H.fromSEXP [r| n_hs * fact_hs(n_hs -1) |]
