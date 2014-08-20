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

preserveDirectory :: IO a -> IO a
preserveDirectory =
 bracket getCurrentDirectory setCurrentDirectory . const

#if R_VERSION >= R_Version(3, 1, 0)
foreign import ccall "&R_PPStackTop" ppStackTop :: Ptr Int
#endif

assertBalancedStack :: IO () -> IO ()
#if R_VERSION >= R_Version(3, 1, 0)
assertBalancedStack m = do
   i <- peek R.ppStackTop
   m
   j <- peek R.ppStackTop
   assertEqual "protection stack should be balanced" i j
#else
assertBalancedStack m = do
    putStrLn "Warning: Cannot check stack balance on R < 3.1. Disabling check."
    m
#endif

stackBalanced :: IO () -> IO ()
stackBalanced f = f
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
    ]
