{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.GC
  ( tests )
  where

import           Control.Memory.Region
import           H.Prelude
import qualified Foreign.R as R
import qualified Foreign.R.Type as SingR
import           Language.R.QQ

import Control.Exception (bracket)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import System.Directory

import System.Mem (performMajorGC)

-- These tests only work with a version of R compiled
-- with --enable-strict-barrier.

tests :: TestTree
tests = testGroup "Automatic values"
    [ testCase "Live automatic not collected by GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "Automatic value was not collected" . isInt) =<<) $ do
            runRegion $ do
              x <- automatic =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V 'R.Int))
              io $ R.gc
              return $ R.typeOf x
    , testCase "Dead automatic collected by GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "Automatic value was collected" . not . isInt) =<<) $ do
           runRegion $ do
              _ <- [r| gctorture(TRUE) |]
              x <- automatic =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V 'R.Int))
              y <- return $ R.release x
              io $ performMajorGC
              _ <- io $ R.allocList 1
              return $! R.typeOf y
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
