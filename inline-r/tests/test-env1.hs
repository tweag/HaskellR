module Main where

import qualified Language.R.Instance as R

import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "initialize respects R_LIBS env" $ do
            let somePath = "bogusfasdfassomePath"
            setEnv "R_LIBS" somePath
            _ <- R.initialize R.defaultConfig
            (somePath @=?) =<< getEnv "R_LIBS"

main :: IO ()
main = defaultMain tests
