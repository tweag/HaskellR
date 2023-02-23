{-# LANGUAGE QuasiQuotes #-}
module Main where

import H.Prelude as H
import qualified Language.R.Instance as R

import System.Environment.Blank as BlankEnv
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "blank R_LIBS does not affect R's stdlib" $ do
            -- use an explcitly blank env R_LIBS="", see `System.Environment.setEnv`
            BlankEnv.setEnv "R_LIBS" "" True
            _ <- R.initialize R.defaultConfig
            ("TRUE" @=?) =<< runRegion
                (fromSomeSEXP <$> [r| deparse(.libPaths() == normalizePath(.Library,winslash="/")) |] :: R s String)

main :: IO ()
main = defaultMain tests
