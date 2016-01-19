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

import           Control.Memory.Region

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Foreign


#include <Rversion.h>

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

-- XXX these tests are only effective when using a "hardened" version of
-- R compiled with --enable-strict-barrier enabled, and with the R_GCTORTURE
-- environment variable set.

tests :: TestTree
tests = testGroup "regions"
    [ testCase "qq-object-live-inside-extend" $
      assertBalancedStack $
        runRegion $ do
          R.SomeSEXP x <- [r| 1 |]
          _ <- [r| gc() |]
          io $ assertEqual "value is protected" R.Real (R.typeOf x)
    , testCase "mksexp-object-live-inside-extend" $
      assertBalancedStack $
        runRegion $ do
          x <- mkSEXP (1::Int32)
          _ <- [r| gc() |]
          io $ assertEqual "value is protected" R.Int (R.typeOf x)
    , testCase "runRegion-no-leaked-thunks" $
        ((8 @=?) =<<) $ do
          z <- runRegion $ fmap dynSEXP [r| 5+3 |]
          _ <- unsafeToIO ([r| gc() |] :: R V (R.SomeSEXP V))
          return (z::Int32)
    ]
