{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Test.Regions
  ( tests )
  where

import           H.Prelude
import qualified Foreign.R as R
import           Language.R.QQ

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Foreign

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Exception (bracket)

#include <Rversion.h>

preserveDirectory :: IO a -> IO a
preserveDirectory =
 bracket getCurrentDirectory setCurrentDirectory . const

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 1, 0)
foreign import ccall "&R_PPStackTop" ppStackTop :: Ptr Int
#endif

assertBalancedStack :: IO () -> IO ()
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 1, 0)
assertBalancedStack m = do
   i <- peek ppStackTop
   m
   j <- peek ppStackTop
   assertEqual "protection stack should be balanced" i j
#else
assertBalancedStack m = do
    putStrLn "Warning: Cannot check stack balance on R < 3.1. Disabling check."
    m
#endif

tests :: TestTree
tests = testGroup "regions"
    [ testCase "qq-dont-leak" $
      preserveDirectory $ assertBalancedStack $
        runRegion $ do
          _ <- [r| gctorture(TRUE) |]
          R.SomeSEXP x <- [r| 1 |]
          _ <- io $ R.allocList 1
          io $ assertEqual "value is protected" R.Real (R.typeOf x)
          _ <- [r| gctorture(FALSE) |]
          return ()
    , testCase "mksexp-dont-leak" $
      preserveDirectory $ assertBalancedStack $
        runRegion $ do
          _ <- [r| gctorture(TRUE) |]
          x <- mkSEXP (1::Int32)
          _ <- io $ R.allocList 1
          io $ assertEqual "value is protected" R.Int (R.typeOf x)
          _ <- [r| gctorture(FALSE) |]
          return ()
    , testCase "runRegion-no-leaked-thunks" $
      preserveDirectory $
        ((8 @=?) =<<) $ do
          runRegion $ do
            _ <- [r| gctorture(TRUE) |]
            return ()
          z <- runRegion $ do
             fmap dynSEXP [r| 5+3 |]
          runRegion $ do
            _ <- io $ R.allocList 1
            _ <- [r| gctorture(FALSE) |]
            return ()
          return (z::Int32)
    , testCase "withRegion-simple" $
      preserveDirectory $ assertBalancedStack $
        runRegion $ do
          _ <- mkSEXP (1 :: Int32)
          withRegion $ do
            _ <- mkSEXP (1 :: Int32)
            pure ()
    , testCase "withRegion-deep" $
      preserveDirectory $ assertBalancedStack $
        runRegion $ do
          x <- mkSEXP (1 :: Int32)
          withRegion $ withRegion $ withRegion $ do
            y <- mkSEXP (1 :: Int32)
            let x' = fromSEXP x :: Int32
                y' = fromSEXP y :: Int32
            io $ assertEqual "fromSEXP in withRegion" x' y'
    ]
