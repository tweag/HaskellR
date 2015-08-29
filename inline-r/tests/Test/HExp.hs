{-# LANGUAGE QuasiQuotes #-}
module Test.HExp ( tests ) where

import           Language.R.HExp
import           Foreign.R as R

import Foreign.C

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "hexp"
    [ testGroup "Cyclyc structures"
        [ testCase "naked-cyclic-structure" $
            R.withProtected (withCString "test" R.mkChar) $ \chr -> do
              R.withProtected (selfSymbol chr) $ \slf -> do
                assertBool "selfSymbol==selfSymbol" (hexp slf === hexp slf)
        ]
  ]
