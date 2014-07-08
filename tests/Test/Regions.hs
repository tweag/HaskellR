{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Regions
  ( tests ) where

import H.Prelude
import Language.R.QQ
import Language.R.Literal
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
          R.SomeSEXP y <- R.protectSome =<< [r| as.integer(42)  |]
          return (fromSEXP (R.unSEXP y))
  ]
  where
    isInt (R.Int) = True
    isInt _ = False
    -- double initialization, but it's safe
