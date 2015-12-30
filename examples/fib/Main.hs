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
  H.io $ putStrLn "[r| neg_hs(TRUE, 5L) |]"
  H.print =<< [r| neg_hs(TRUE, 5L) |]
  H.io $ putStrLn "[r| neg_hs(FALSE, 6L) |]"
  H.print =<< [r| neg_hs(FALSE, 6L) |]
  H.io $ putStrLn "[r| neg_hs(NA, 7L) |]"
  H.print =<< [r| neg_hs(NA, 7L) |]
  H.io $ putStrLn "[r| fib_hs(1L) |]"
  H.print =<< [r| fib_hs(1L) |]
  H.io $ putStrLn "[r| fib_hs(10L) |]"
  H.print =<< [r| fib_hs(10L) |]
  H.io $ putStrLn "[r| fact_hs(0L) |]"
  H.print =<< [r| fact_hs(0L) |]
  H.io $ putStrLn "[r| fact_hs(7L) |]"
  H.print =<< [r| fact_hs(7L) |]
  H.io $ putStrLn "[r| factSexp_hs(7L) |]"
  H.print =<< [r| factSexp_hs(7L) |]
