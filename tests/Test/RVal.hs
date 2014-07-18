{-# LANGUAGE DataKinds #-}

module Test.RVal
  ( tests )
  where

import           Foreign.R.GC
import qualified Foreign.R.Internal as R
import qualified Foreign.R.Type as SingR
import Foreign.R.Runner

import Control.Exception (bracket)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import System.Directory


tests :: TestTree
tests = testGroup "HVal"
    [ testCase "RVal is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            unsafeRunInRThread $ do
              x <- newRVal =<< (R.allocVector SingR.SInt 1024 :: IO (R.SEXP R.Int))
              R.gc
              withRVal x (return . R.typeOf)
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
