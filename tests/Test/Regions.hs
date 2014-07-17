{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Regions
  ( tests ) where

import H.Prelude
import Language.R.QQ
import Data.Int

import qualified Foreign.R as R

import Control.Exception (bracket)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import System.Directory

preserveDir :: IO a -> IO a
preserveDir = bracket getCurrentDirectory setCurrentDirectory . const

tests :: TestTree
tests = testGroup "regions"
  [ testCase "region/protect" $ preserveDir $ do
     ((assertBool "RVal was collected") =<<) $ do
        unsafeRunRegion $ do
         _ <- [r| gctorture(TRUE) |]         -- Start gctorture
         y <- mkSEXP (42::Int32)             -- allocate a varaible inside a region
         _ <-[r| 1 + 1|]                     -- force gc
         return $ isInt (R.typeOf y)         -- Test is value was not freed
  , testCase "region/return" $ preserveDir $ do
     ((assertBool "value contain thunks to protected data" . (==) (42::Int32)) =<<) $ do
        unsafeRunRegion $ do
          _ <- [r| gctorture(TRUE) |]
          y <- [r| as.integer(42)  |]
          return (fromSEXP (R.cast R.Int y) :: Int32)
  , testCase "region/subregion" $ preserveDir $ do
      ((assertBool "not Nil") =<<) $ do
        unsafeRunRegion $ do
          _ <- [r| gctorture(TRUE) |]
	  x <- mkSEXP (42::Int32)
	  newRegion $ \(SubRegion wtn) -> do
	    y <- wtn $ R.getAttribute x
	    return $ isNil (R.typeOf y)
  , testCase "region/subregion-improved" $ preserveDir $ do
      ((assertBool "not List") =<<) $ do
        unsafeRunRegion $ do
          _ <- [r| gctorture(TRUE) |]
	  x <- mkSEXP (42::Int32)
	  newRegion $ \(SubRegion wtn) -> do
	    l <- wtn $ R.allocList 1
	    wtn $ R.setAttribute x l
	    y <- wtn $ R.getAttribute x
	    return $ isList (R.typeOf y)
  ]
  where
    isInt (R.Int) = True
    isInt _ = False
    isNil (R.Nil) = True
    isNil _ = False
    isList (R.List) = True
    isList _ = False
