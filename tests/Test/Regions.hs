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

-- 196864 - is R-3.1.0
#if R_VERSION >= 196864
foreign import ccall "&R_PPStackTop" ppStackTop :: Ptr Int

stackBalanced :: IO () -> IO ()
stackBalanced f = do
   i <- peek R.ppStackTop
   f
   j <- peek R.ppStackTop
   assertEqual "protection stack should be balanced" i j
#else
stackBalanced :: IO () -> IO ()
stackBalanced f = f
#endif

tests :: TestTree
tests = testGroup "regions"
    [ testCase "qq-dont-leak" $
      preserveDirectory $ stackBalanced $
        runRegion $ do
          _ <- [r| gctorture(TRUE) |]
          R.SomeSEXP x <- [r| 1 |]
          _ <- io $ R.allocList 1
          io $ assertEqual "value is protected" R.Real (R.typeOf x)
          _ <- [r| gctorture(FALSE) |]
          return ()
    , testCase "mksexp-dont-leak" $
      preserveDirectory $ stackBalanced $
        runRegion $ do
          _ <- [r| gctorture(TRUE) |]
          x <- mkSEXP (1::Int32)
          _ <- io $ R.allocList 1
          io $ assertEqual "value is protected" R.Int (R.typeOf x)
          _ <- [r| gctorture(FALSE) |]
          return ()
    ]

