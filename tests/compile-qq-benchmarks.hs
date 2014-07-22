-- Copyright: (C) 2013 Amgen, Inc.
--
-- This program executes the benchmark of the fib function using R and
-- the compile-time qq.
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import Foreign.R as R
import Language.R as R
import H.Prelude as H
import Language.R.QQ

import Criterion.Main
import Data.Int
import Language.Haskell.TH.Quote

import System.FilePath

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

hFib :: SEXP s R.Int -> R s (SEXP s R.Int)
hFib n@(fromSEXP -> (0 :: Int32)) = fmap (flip R.asTypeOf n) [r| as.integer(0) |]
hFib n@(fromSEXP -> (1 :: Int32)) = fmap (flip R.asTypeOf n) [r| as.integer(1) |]
hFib n                            = H.withProtected (return n) $ const $
    fmap (flip R.asTypeOf n) [r| as.integer(hFib_hs(as.integer(n_hs - 1)) + hFib_hs(as.integer(n_hs - 2))) |]

main :: IO ()
main = do
    H.withEmbeddedR H.defaultConfig $ runRegion $ do
      _ <- $(quoteExp (quoteFile r) ("tests" </> "R" </> "fib.R"))
      io $ defaultMain [
             bgroup "fib"
                   [ bench "pure Haskell" $
                       nf fib 18
                   , bench "compile-time-qq" $
                       unsafeRToIO [r| fib(18) |]
                   , bench "compile-time-qq-hybrid" $
                       unsafeRToIO $ hFib $! mkSEXP (18 :: Int32)
                   ]
               ]
