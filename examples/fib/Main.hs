{-# LANGUAGE QuasiQuotes #-}
module Main where

import Fib

import qualified H.Prelude as H
import           Language.R.QQ

main :: IO ()
main = H.withEmbeddedR H.defaultConfig $ H.runRegion $ do
  let p x = [r| print(x_hs) |] >> return ()
  p =<< [r| "test" |]
  p =<< [r| 1+2 |]
  H.io $ putStrLn "[r| neg_hs(TRUE, 5L) |]"
  p =<< [r| neg_hs(TRUE, 5L) |]
  H.io $ putStrLn "[r| neg_hs(FALSE, 6L) |]"
  p =<< [r| neg_hs(FALSE, 6L) |]
  H.io $ putStrLn "[r| neg_hs(NA, 7L) |]"
  p =<< [r| neg_hs(NA, 7L) |]
  H.io $ putStrLn "[r| fib_hs(1L) |]"
  p =<< [r| fib_hs(1L) |]
  H.io $ putStrLn "[r| fib_hs(10L) |]"
  p =<< [r| fib_hs(10L) |]
  H.io $ putStrLn "[r| fact_hs(0L) |]"
  p =<< [r| fact_hs(0L) |]
  H.io $ putStrLn "[r| fact_hs(7L) |]"
  p =<< [r| fact_hs(7L) |]
  H.io $ putStrLn "[r| factSexp_hs(7L) |]"
  p =<< [r| factSexp_hs(7L) |]
