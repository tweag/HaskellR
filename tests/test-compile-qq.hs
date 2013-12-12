-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Tests. Run H on a number of R programs of increasing size and complexity,
-- comparing the output of H with the output of R.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Foreign.R as R ( SEXP )
import H.Prelude as H
import Language.R.QQ
import Data.Int

import System.Environment
import System.Exit ( exitFailure )
import System.Process ( readProcess )

main :: IO ()
main = do
    args <- getArgs
    if null args
    then do
      output <- readProcess "dist/build/test-compile-qq/test-compile-qq" ["test"] ""
      golden <- readFile "tests/ghci/compile-qq.ghci.golden.output"
      putStr "Testing compile time qq: "
      if (output /= golden)
      then do
        putStrLn $ unlines $
          [ "outputs don't match "
          , ""
          , "expected: "
          , golden
          , ""
          , show golden
          , ""
          , "H:"
          , ""
          , output
          , ""
          , show output
          ]
        exitFailure
      else putStrLn "OK"
    else rTests

rTests :: IO ()
rTests = H.initialize H.defaultConfig >>= \rEnv -> runR rEnv $ do

    -- Should be: [1] 1
    H.print =<< [r| 1 |]

    -- Should be: [1] 2
    H.print =<< [r| 1 + 1 |]

    -- Should be: [1] "1" "2" "3"
    H.print =<< [r| c(1,2,"3") |]

    -- Should be: [1] 2
    H.print =<< [r| x <- 2 |]

    -- Should be: [1] 3
    H.print =<< [r| x+1 |]

    ---- Should be: [1] 6
    let y = (5::Double)
    H.print =<< [r| y_hs + 1 |]

    -- Should be: Closure ???
    _ <- [r| function(y) y_hs + y |]

    -- Should be 8
    _ <- [r| z <- function(y) y_hs + y |]
    H.print =<< [r| z(3) |]

    -- Should be [1] 1 2 3 4 5 6 7 8 9 10
    H.print =<< [r| y <- c(1:10) |]
    let foo1 = (\x -> (return $ x+1 :: H.R Double))
    let foo2 = (\x -> (return $ map (+1) x :: H.R [Int32]))

    -- Should be [1] 2
    H.print =<< [r| (function(x).Call(foo1_hs,x))(2) |]

    -- Should be [1] 2 3 4 5 6 7 8 9 10 11
    H.print =<< [r| (function(x).Call(foo2_hs,x))(y) |]

    -- Should be [1] 43
    H.print =<< [r| x <- 42 ; x + 1 |]

    -- Should be [1] 1 2 3
    let xs = [1,2,3]::[Double]
    H.print =<< [r| xs_hs |]

    -- Should be [1] 8
    H.print =<< [r| foo1_hs(7) |]

    -- Should be NULL
    io $ H.runInRThread (runR rEnv $ H.print H.nilValue)

    -- Should be [1] 3
    let foo3 = (\n -> fmap H.fromSEXP [r| n_hs |]) :: Int32 -> H.R Int32
    H.print =<< [r| foo3_hs(as.integer(3)) |]

    -- | should be 3
    let foo4 = (\n m -> return $ n + m) :: Double -> Double -> H.R Double
    H.print =<< [r| foo4_hs(33, 66) |]

    -- Should be [1] 120 but it doesn't work
    let fact n = if n == (0 :: Int32) then (return 1 :: H.R Int32) else fmap H.fromSEXP [r| as.integer(n_hs * fact_hs(as.integer(n_hs - 1))) |]
    H.print =<< [r| fact_hs(as.integer(5)) |]

    -- Should be [1] 29
    let foo5  = (\n -> return (n+1)) :: Int32 -> R Int32
    let apply = (\n m -> [r| .Call(n_hs, m_hs) |]) :: R.SEXP a -> Int32 -> R (R.SEXP b)
    H.print =<< [r| apply_hs(foo5_hs, as.integer(28) ) |]

    sym <- H.symbol "blah"
    H.print sym

    -- Should be [1] 100
    _ <- [r| `+` <- function(x,y) x * y |]
    H.print =<< [r| 10 + 10 |]

    -- Should be [1] 20
    H.print =<< [r| base::`+`(10,10) |]

    -- restore usual meaning of `+`
    _ <- [r| `+` <- base::`+` |]
    return ()
