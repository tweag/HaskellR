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
    , testGroup "HExp-support-unknown types"
        [ testCase "equal" $ do
            s1 <- unsafeRToIO [r| 1 |]
            s2 <- unsafeRToIO [r| 1 |]
            unSomeSEXP s1 $ \a -> unSomeSEXP s2 $ \b ->
              assertBool "Can compare" $ H.hexp a === H.hexp b
        , testCase "not equal" $ do
            s1 <- unsafeRToIO [r| 1 |]
            s2 <- unsafeRToIO [r| 2 |]
            unSomeSEXP s1 $ \a -> unSomeSEXP s2 $ \b ->
              assertBool "Can compare" $ not $ H.hexp a === H.hexp b
        , testCase "different types" $ do
            s1 <- unsafeRToIO [r| 1 |]
            s2 <- unsafeRToIO [r| as.integer(1) |]
            unSomeSEXP s1 $ \a -> unSomeSEXP s2 $ \b ->
              assertBool "Can compare" $ not $ H.hexp a === H.hexp b
        , testCase "same after evaluation" $ do
            s1 <- unsafeRToIO [r| 1 |]
            s2 <- unsafeRToIO [r| 0+1 |]
            unSomeSEXP s1 $ \a -> unSomeSEXP s2 $ \b ->
              assertBool "Can compare" $ H.hexp a === H.hexp b
        ]
    ]
