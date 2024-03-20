-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Main test suite for inline-r. Pass --torture on command-line or set
-- R_GCTORTURE environment variable to perform memory tests. They will be
-- ignored otherwise. Only pass --torture when linking against a version of
-- R compiled with the --enable-strict-barrier configure flag turned on.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--ple" @-}
module Main where

-- import qualified Test.Constraints
-- import qualified Test.Event
import qualified Test.FunPtr
-- import qualified Test.GC
-- import qualified Test.Regions
import qualified Test.Vector
-- import qualified Test.Matcher

import H.Prelude
import qualified Foreign.R as R
import qualified Language.R.Instance as R
    ( initialize
    , defaultConfig )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Control.Applicative
import Control.Concurrent
import Control.Exception (handle)
import Control.Memory.Region
import Control.Monad (void)
import Data.List
import Data.Singletons (sing)
import Data.Vector.SEXP (indexM)
import qualified Data.Vector.SEXP
import qualified Control.Monad.Reader
import qualified Data.IORef
import qualified Foreign.C.String
import qualified GHC.ST
import Foreign hiding (void)
import qualified Foreign.R.Internal as R (checkSEXPTYPE)
import System.Environment (getArgs, lookupEnv, withArgs)
import qualified Text.Heredoc as Heredoc
import Prelude -- Silence AMP warning

inVoid :: R V z -> R V z
inVoid = id

-- XXX: Why peek cannot be inlined?
{-@ assume myPeek :: t:R.SEXPTYPE -> Ptr (TSEXP s t) -> IO (TSEXP s t) @-}
{-@ ignore myPeek @-}
myPeek :: R.SEXPTYPE -> Ptr (SEXP s) -> IO (SEXP s)
myPeek _ x = peek x

tests :: Bool -> TestTree
tests torture = testGroup "Unit tests"
  [ testCase "fromSEXP . mkSEXP" $ do
      z <- fromSEXP <$> mkSEXPIO (2 :: Double)
      (2 :: Double) @=? z
  , testCase "HEq HExp" $ do
      -- XXX ideally randomly generate input.
      let x = 2 :: Double
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "reflexive" $
          let s = hexp z in s == s
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "symmetric" $
          let s1 = hexp z
              s2 = hexp z
          in s1 == s2 && s2 == s1
      R.withProtected (mkSEXPIO x) $ \z ->
        assertBool "transitive" $
          let s1 = hexp z
              s2 = hexp z
              s3 = hexp z
          in s1 == s2 && s2 == s3 && s1 == s3
  , testCase "Haskell function from R" $ do
      (((3::Double) @=?) =<<) $ fmap fromSEXP $
          alloca $ \p -> do
            e <- myPeek R.Env R.globalEnv
            R.withProtected (mkSEXPIO $ \x -> return $ x + 1 :: R s Double) $
              \sf -> R.withProtected (mkSEXPIO (2::Double)) $ \d ->
                       R.withProtected (R.lang2 sf d) (unsafeRunRegion . eval)
                       >>= \s ->
                              R.checkSEXPTYPE (R.Real) <$>
                              R.tryEval s (R.release e) p
  , testCase "Weak Ptr test" $ runRegion $ do
      key  <- [r| new.env() |]
      val  <- [r| new.env() |]
      True <- return $ R.typeOf val == R.Env
      n    <- io $ R.release <$> myPeek R.Nil R.nilValue
      rf   <- io $ R.mkWeakRef key val n True
      -- XXX: Here LH can prove that the case is complete!
      True <- case hexp rf of
                WeakRef a b c _ -> do
                  True <- return $ (R.unsexp a) == (R.unsexp key)
                  True <- return $ (R.unsexp b) == (R.unsexp val)
                  return $ (R.unsexp c) == (R.unsexp n)
      return ()
  , testCase "Hexp works" $
      (((42::Double) @=?) =<<) $ runRegion $ do
         y <- R.checkSEXPTYPE R.Real
                     <$> mkSEXP (42::Double)
         case hexp y of
           Real s -> s `indexM` 0
--  , Test.Constraints.tests
  , Test.FunPtr.tests
--  , (if torture then id else ignoreTest) Test.GC.tests
--  , (if torture then id else ignoreTest) Test.Regions.tests
  , Test.Vector.tests
--  , Test.Event.tests
--  , Test.Matcher.tests
    -- This test helps compiling quasiquoters concurrently from
    -- multiple modules. This in turns helps testing for race
    -- conditions when initializing R from multiple threads.
  , testCase "qq/concurrent-initialization" $ runRegion $ [r| 1 |] >> return ()
  , testCase "sanity check " $ return ()
  , testCase "cancel works" $ do
      void $ forkIO $ cancel
      handle (\RError{} -> return ())
             (runRegion $ void $ [r| while(1){}; |])
  , testGroup "collectAntis"
    [ testCase "Succeeds on a simple expression" $ do
        result <- collectAntis "foo_hs + bar + baz_hs"
        case result of
          Left err -> assertFailure err
          Right ids -> sort ids @?=
            sort ["foo_hs", "baz_hs"]
    , testCase "Returns an error on invalid R code" $ do
        result <- collectAntis "foo_hs bar"
        case result of
          Left err -> assertBool "Unrecognized error message" $
            "unexpected symbol" `isInfixOf` err
          Right _ -> assertFailure "Unexpected success"
    , testCase "Succeeds on a more complicated expression and ignores symbols inside strings" $ do
        result <- collectAntis [Heredoc.here|
            function () {
              r <- fft(foo_hs) + bar_hs + "baz_hs"
              f(r, quux_hs)
            }
          |]
        case result of
          Left err -> assertFailure err
          Right ids -> sort ids @?=
            sort ["foo_hs", "bar_hs", "quux_hs"]
    ]
  ]

main :: IO ()
main = do
    _ <- R.initialize R.defaultConfig
    argv <- getArgs
    -- Assume gctorture() step size of 1.
    let tortureCLI = "1" <$ find (== "--torture") argv
    tortureEnv <- lookupEnv "R_GCTORTURE"
    torture <- case tortureCLI <|> tortureEnv of
      Nothing -> return False
      Just x -> do
        -- gctorture turned on. So assume moreover --enable-strict-barrier.
        let step = read x :: Int32
        putStrLn "WARNING: gctorture() turned on.\n\
                 \    Tests will fail if R not compiled with --enable-strict-barrier."
        runRegion $ void [r| gctorture2(step = step_hs, inhibit_release = TRUE) |]
        return True
    withArgs (delete "--torture" argv) $
      defaultMain (tests torture)
