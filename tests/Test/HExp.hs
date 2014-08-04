{-# LANGUAGE QuasiQuotes #-}
module Test.HExp ( tests ) where

import H.Constraints
import qualified Language.R.HExp as H
import           Language.R as R
import           Foreign.R as R
import           Language.R.QQ

import Foreign.C

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "hexp"
    [ testGroup "Cyclyc structures"
        [ testCase "naked-cyclic-structure" $
            R.withProtected (withCString "test" R.mkChar) $ \chr -> do
              R.withProtected (H.selfSymbol chr) $ \slf -> do
                assertBool "selfSymbol==selfSymbol" (H.hexp slf === H.hexp slf)
        ]
  ]
