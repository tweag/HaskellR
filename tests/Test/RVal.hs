{-# LANGUAGE DataKinds #-}

module Test.RVal
  ( tests )
  where

import           Control.Memory.Region
import           H.Prelude
import qualified Foreign.R as R
import qualified Foreign.R.Type as SingR

import Control.Exception (bracket)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import System.Directory


tests :: TestTree
tests = testGroup "HVal"
    [ testCase "RVal is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            unsafeRunInRThread $ unsafeRToIO $ do
              x <- newRVal =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V R.Int))
              io $ R.gc
              withRVal x (return . R.typeOf)
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
