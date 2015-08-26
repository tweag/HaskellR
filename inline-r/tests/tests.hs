-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Tests. Run H on a number of R programs of increasing size and complexity,
-- comparing the output of H with the output of R.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Test.Constraints
import qualified Test.Event
import qualified Test.FunPtr
import qualified Test.HExp
import qualified Test.GC
import qualified Test.Regions
import qualified Test.Vector

import H.Prelude
import Foreign.R.Constraints
import qualified Language.R.HExp as H
import qualified Foreign.R as R
import qualified Language.R.Instance as R
    ( initialize
    , defaultConfig )
import qualified Language.R as R ( r2 )
import           Language.R.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 (pack)
import           Data.Vector.Generic (basicUnsafeIndexM)
import           Data.Singletons (sing)
import Foreign

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "fromSEXP . mkSEXP" $ do
      z <- fromSEXP <$> mkSEXPIO (2 :: Double)
      (2 :: Double) @=? z
  , testCase "HEq HExp" $ do
      -- XXX ideally randomly generate input.
      let x = 2 :: Double
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "reflexive" $
          let s = H.hexp z in s === s
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "symmetric" $
          let s1 = H.hexp z
              s2 = H.hexp z
          in s1 === s2 && s2 === s1
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "transitive" $
          let s1 = H.hexp z
              s2 = H.hexp z
              s3 = H.hexp z
          in s1 === s2 && s2 === s3 && s1 === s3
  , testCase "Haskell function from R" $ do
--      (("[1] 3.0" @=?) =<<) $
--        fmap ((\s -> trace s s).  show . toHVal) $ alloca $ \p -> do
      (((3::Double) @=?) =<<) $ fmap fromSEXP $
          alloca $ \p -> do
            e <- peek R.globalEnv
            R.withProtected (mkSEXPIO $ \x -> return $ x + 1 :: R s Double) $
              \sf -> R.withProtected (mkSEXPIO (2::Double)) $ \d ->
                      R.r2 (Data.ByteString.Char8.pack ".Call") sf d
                      >>= \(R.SomeSEXP s) -> R.cast  (sing :: R.SSEXPTYPE R.Real)
                                                     <$> R.tryEval s (R.release e) p
  , testCase "Weak Ptr test" $ runRegion $ do
      key  <- mkSEXP (return 4 :: R s Int32)
      val  <- mkSEXP (return 5 :: R s Int32)
      True <- return $ R.typeOf val == R.ExtPtr
      n    <- H.unhexp H.Nil
      rf   <- io $ R.mkWeakRef key val n True
      True <- case H.hexp rf of
                H.WeakRef a b c _ -> do
                  True <- return $ (R.unsexp a) == (R.unsexp key)
                  True <- return $ (R.unsexp b) == (R.unsexp val)
                  return $ (R.unsexp c) == (R.unsexp n)
                _ -> error "unexpected type"
      return ()
  , testCase "Hexp works" $
      (((42::Double) @=?) =<<) $ runRegion $ do
         y <- R.cast (sing :: R.SSEXPTYPE R.Real) . R.SomeSEXP
                     <$> mkSEXP (42::Double)
         case H.hexp y of
           H.Bytecode -> return 15
           H.Real s -> io $ basicUnsafeIndexM s 0
  , Test.Constraints.tests
  , Test.FunPtr.tests
  , Test.HExp.tests
  , Test.GC.tests
  , Test.Regions.tests
  , Test.Vector.tests
  , Test.Event.tests
    -- This test helps compiling quasiquoters concurrently from
    -- multiple modules. This in turns helps testing for race
    -- conditions when initializing R from multiple threads.
  , testCase "qq/concurrent-initialization" $ unsafeRToIO $ [r| 1 |] >> return ()
  , testCase "sanity check " $ return ()
  ]

main :: IO ()
main = do
    _ <- R.initialize R.defaultConfig
    defaultMain tests
