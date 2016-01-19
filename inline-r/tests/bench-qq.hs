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

import Control.Applicative
import Control.Memory.Region
import Criterion.Main
import Data.Int
import Language.Haskell.TH.Quote

import System.FilePath
import Prelude -- Silence AMP warning

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

hFib :: SEXP s 'R.Int -> R s (SEXP s 'R.Int)
hFib n@(H.fromSEXP -> 0 :: Int32) = fmap (flip R.asTypeOf n) [r| 0L |]
hFib n@(H.fromSEXP -> 1 :: Int32) = fmap (flip R.asTypeOf n) [r| 1L |]
hFib n = (`R.asTypeOf` n) <$> [r| hFib_hs(n_hs - 1L) + hFib_hs(n_hs - 2L) |]

inVoid :: R V s -> R V s
inVoid = id

main :: IO ()
main = do
    H.withEmbeddedR H.defaultConfig $ runRegion $ do
      _ <- $(quoteExp (quoteFile r) ("tests" </> "R" </> "fib.R"))
      io $ defaultMain [
             bgroup "fib"
                   [ bench "pure Haskell" $
                       nf fib 18
                   , bench "compile-time-qq" $
                       nfIO $ unsafeToIO $ inVoid $ do
                         [r| fib <<- function(n) {if (n == 1) return(1); if (n == 2) return(2); return(fib(n-1)+fib(n-2))} |] >> return ()
                         [r| fib(18) |]
                   , bench "compile-time-qq-hybrid" $
                       nfIO $ unsafeToIO $ inVoid $ hFib =<< mkSEXP (18 :: Int32)
                   ]
               ]
