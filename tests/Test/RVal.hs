{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.RVal
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


tests :: TestTree
tests = testGroup "HVal"
    [ testCase "RVal is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            unsafeRunInRThread $ unsafeRToIO $ do
              x <- newRVal =<< io (R.allocVector SingR.SInt 1024 :: IO (R.SEXP V R.Int))
              io $ R.gc
              withRVal x (return . R.typeOf)
    , testCase "RVal works with quasi-quotes" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            unsafeRunInRThread $ unsafeRToIO $ do
              x <- newSomeRVal =<< [r| as.integer(1024) |]
              io $ R.gc
              withSomeRVal x (\(R.SomeSEXP s) -> return $ R.typeOf s)
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
