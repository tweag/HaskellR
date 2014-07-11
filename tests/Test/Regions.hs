{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Regions
  ( tests
  ) where

import H.Prelude
import Language.R.QQ
import Data.Int

import qualified Foreign.R as R

import Control.Exception (bracket)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import System.Directory

tests :: TestTree
tests = testGroup "regions"
    [ testCase "RVal is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
	    -- double initialization is not supported!
	    unsafeRunRegion $ do
	      _ <- [r| gctorture(TRUE) |]  -- Start gctorture
	      y <- runRegion $ newRVal =<< mkSEXP (42::Int32)
	      _ <- [r| 1 + 1 |]            -- force gc
	      withRVal y (const $ return . R.typeOf)
    , testCase "It's possible to share RVal between regions" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        unsafeRunRegion $ do
	  y <- runRegion $ newRVal =<< mkSEXP (42::Int32)
	  runRegion $ withRVal y (const . const $ return ())
    , testCase "Lift value into the withRVal region" $ do
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
	unsafeRunRegion $ do
	    y <- mkSEXP (42::Int32) :: R s (R.SEXP s R.Int)
	    x <- newRVal =<< mkSEXP (42::Int32)
	    _ <- runRegion $ withRVal x (const . const $ return y)
	    return ()
    , testCase "Lift value into the withRVal region" $ do
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
	unsafeRunRegion $ do
	  runRegion $ do
	    y <- protect =<< mkSEXP (42::Int32) :: R s (R.SEXP s R.Int)
	    runRegion $ do
	      x <- newRVal =<< mkSEXP (42::Int32)
	      _ <- runRegion $ withRVal x $ \(SubRegion wt) x1 -> do
	         let f a b = [r| a_hs + b_hs |]
		 wt $(f x1) y
	      return ()
    ]
  where
    isInt (R.Int) = True
    isInt _       = False
            -- double initialization, but it's safe
