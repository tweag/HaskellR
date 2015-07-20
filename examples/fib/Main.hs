{-# LANGUAGE QuasiQuotes #-}
module Main
  where

import Fib

import qualified H.Prelude as H
import           Language.R.QQ

main :: IO ()
main = H.withEmbeddedR H.defaultConfig $ H.runRegion $ do
  H.print =<< [r| "test" |]
  H.print =<< [r| 1+2 |]
  H.io $ putStrLn "[r| neg_hs(TRUE, as.integer(5)) |]"
  H.print =<< [r| neg_hs(TRUE, as.integer(5)) |]
  H.io $ putStrLn "[r| neg_hs(FALSE, as.integer(6)) |]"
  H.print =<< [r| neg_hs(FALSE, as.integer(6)) |]
  H.io $ putStrLn "[r| neg_hs(NA, as.integer(7)) |]"
  H.print =<< [r| neg_hs(NA, as.integer(7)) |]
  H.io $ putStrLn "[r| fib_hs(as.integer(1)) |]"
  H.print =<< [r| fib_hs(as.integer(1)) |]
  H.io $ putStrLn "[r| fib_hs(as.integer(10)) |]"
  H.print =<< [r| fib_hs(as.integer(10)) |]
  H.io $ putStrLn "[r| fact_hs(as.integer(0)) |]"
  H.print =<< [r| fact_hs(as.integer(0)) |]
  H.io $ putStrLn "[r| fact_hs(as.integer(7)) |]"
  H.print =<< [r| fact_hs(as.integer(7)) |]
  H.io $ putStrLn "[r| factSexp_hs(as.integer(7)) |]"
  H.print =<< [r| factSexp_hs(as.integer(7)) |]

