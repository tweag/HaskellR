{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Regions
  ( tests )
  where

import           H.Prelude
import qualified Foreign.R as R
import           Language.R.QQ

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Foreign

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Exception (bracket)
preserveDirectory :: IO a -> IO a
preserveDirectory =
 bracket getCurrentDirectory setCurrentDirectory . const

tests :: TestTree
tests = testGroup "regions"
    [ testCase "qq-dont-leak" $
      preserveDirectory $ do
        i <- peek R.ppStackTop
        runRegion $ do
          _ <- [r| 1 |]
          j <- io $ peek R.ppStackTop
          io $ assertEqual "protection stack should grow" (succ i) j
        j <- peek R.ppStackTop
        assertEqual "protection stack should be balanced" i j
    , testCase "mksexp-dont-leak" $
      preserveDirectory $ do
        i <- peek R.ppStackTop
        runRegion $ do
          _ <- mkSEXP (1::Int32)
          j <- io $ peek R.ppStackTop
          io $ assertEqual "protection stack should grow" (succ i) j
        j <- peek R.ppStackTop
        assertEqual "protection stack should be balanced" i j
    ]
