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

tests :: TestTree
tests = testGroup "HVal"
    [ testCase "Preserved value is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "Preserved value was collected" . isInt) =<<) $ do
            unsafeRunInRThread $ unsafeRToIO $ do
              x <- preserve =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V R.Int))
              io $ R.gc
              return $ R.typeOf x
    , testCase "Preserved value works after release" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "Preserved value was collected" . isInt) =<<) $ do
           unsafeRunInRThread $ runRegion $ do
              _ <- [r| gctorture(TRUE) |]
              x <- preserve =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V R.Int))
              y <- return $ R.release x
              io $ performMajorGC
              _ <- io $ R.allocList 1
              return $! R.typeOf y
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
