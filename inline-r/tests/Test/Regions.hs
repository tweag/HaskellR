{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Test.Regions
  ( tests )
  where

import           Control.Memory.Region
import qualified Foreign.R as R
import           H.Prelude
import           Language.R.HExp (hexp, (===))
import           Language.R.QQ

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Foreign

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)

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
          _ <- unsafeRToIO $ [r| gc() |]
          return (z::Int32)
    , testCase "withRegion-object-live-inside-extent" $
      runRegion $ withRegion $ do
        R.SomeSEXP x <- [r| 1 |]
        _ <- [r| gc () |]
        io $ assertEqual "value is protected" R.Real (R.typeOf x)
    , testCase "withRegion-object-dead-outside-extent" $
      runRegion $ do
        mv <- io $ newEmptyMVar
        -- _ instead of () leads to a mysterious type error in GHC 7.10.
        () <- withRegion $ do
            env <- [r| new.env() |]
            let f  (R.SomeSEXP _) = do
                  io $ putMVar mv ()
                  return nilValue :: R G (R.SEXP G 'R.Nil)
            _ <- [r| reg.finalizer(env_hs, function(x)f_hs(x)) |]
            return ()
        _ <- [r| gc() |]
        io $ takeMVar mv
    , testCase "withRegion-parent-region-unaffected" $
      runRegion $ do
        R.SomeSEXP x <- [r| 1 |]
        () <- withRegion $ do
          R.SomeSEXP y <- [r| 1 |]
          io $ assert $ hexp (subr x) === hexp y
        _ <- [r| gc() |]
        io $ assertEqual "value is protected" R.Real (R.typeOf x)
    ]
