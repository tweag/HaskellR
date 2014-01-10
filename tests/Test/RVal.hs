{-# LANGUAGE DataKinds #-}

module Test.RVal
  ( tests )
  where

import           H.Prelude
import qualified Foreign.R as R
import qualified Foreign.R.Type as SingR
import qualified Language.R.Instance as R

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HVal"
    [ testCase "SEXP is collected after R GC" $ do
        ((assertBool "SEXP was not collected" . not . isInt) =<<) $ do
          x <- R.allocVector SingR.SInt 1024 :: IO (R.SEXP R.Int)
          R.gc
          return $ R.typeOf x
    , testCase "RVal is not collected by R GC" $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            -- double initialization, but it's safe
            R.runR R.defaultConfig $ do
              x <- newRVal =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP R.Int))
              io $ R.gc
              withRVal x (return . R.typeOf)
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
