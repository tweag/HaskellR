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
        runR $ do
         _ <- [r| gctorture(TRUE) |]         -- Start gctorture
         y <- mkSEXP (42::Int32)             -- allocate a varaible inside a region
         _ <-[r| 1 + 1|]                     -- force gc
         return $ isInt (R.typeOf y)         -- Test is value was not freed
  , testCase "region/return" $ preserveDir $ do
     ((assertBool "value contain thunks to protected data" . (==) (42::Int32)) =<<) $ do
        runR $ do
          _ <- [r| gctorture(TRUE) |]
          y <- [r| as.integer(42)  |]
          return (fromSEXP (R.cast R.Int y) :: Int32)
  , testCase "region/subregion" $ preserveDir $ do
      ((assertBool "not Nil") =<<) $ do
        runR $ do
          _ <- [r| gctorture(TRUE) |]
          x <- mkSEXP (42::Int32)
          newRegion $ \(SubRegion wtn) -> do
            y <- wtn $ R.getAttribute x
            return $ isNil (R.typeOf y)
  , testCase "region/subregion-improved" $ preserveDir $ do
      ((assertBool "not List") =<<) $ do
        runR $ do
          _ <- [r| gctorture(TRUE) |]
          x <- mkSEXP (42::Int32)
          newRegion $ \(SubRegion wtn) -> do
            l <- wtn $ R.allocList 1
            wtn $ R.setAttribute x l
            y <- wtn $ R.getAttribute x
            return $ isList (R.typeOf y)
  ,  testCase "RVal is not collected by R GC" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        ((assertBool "RVal was collected" . isInt) =<<) $ do
          -- double initialization is not supported!
          runR $ do
            _ <- [r| gctorture(TRUE) |]  -- Start gctorture
            y <- runRegion $ newRVal =<< mkSEXP (42::Int32)
            _ <- [r| 1 + 1 |]            -- force gc
            withRVal y (const $ return . R.typeOf)
    , testCase "It's possible to share RVal between regions" $
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        runR $ do
          y <- runRegion $ newRVal =<< mkSEXP (42::Int32)
          runRegion $ withRVal y (const . const $ return ())
    , testCase "Lift value into the withRVal region" $ do
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        runR $ do
            y <- mkSEXP (42::Int32) :: R s (R.SEXP s R.Int)
            x <- newRVal =<< mkSEXP (42::Int32)
            _ <- runRegion $ withRVal x (const . const $ return y)
            return ()
    , testCase "Lift value into the withRVal region" $ do
      bracket getCurrentDirectory setCurrentDirectory $ const $ do
        runR $ do
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
    isInt _ = False
    isNil (R.Nil) = True
    isNil _ = False
    isList (R.List) = True
    isList _ = False
