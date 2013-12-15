module Test.RVal
  ( tests )
  where

import           H.Prelude.RVal
import           H.Monad
import qualified Foreign.R as R
import qualified Language.R.Interpreter as R (initialize, defaultConfig)

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HVal"
    [ testCase "SEXP is collected after R GC" $ do
        ((assertBool "SEXP was not collected" . not . isInt) =<<) $ do
          x <- R.allocVector R.Int 1024
          R.gc
          return $ R.typeOf x
    , testCase "RVal is not collected by R GC" $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
            -- double initialization, but it's safe
            witness <- R.initialize R.defaultConfig
            x <- runR witness $ newRVal =<< io (R.allocVector R.Int 1024)
            R.gc
            runR witness $ withRVal x (return . R.typeOf)
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
