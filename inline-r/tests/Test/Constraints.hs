{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Test.Constraints
  ( tests )
  where

import H.Constraints
import qualified Foreign.R.Type as R

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (guard)

prop_reflexivity :: (a :∈ '[a]) => R.SSEXPTYPE a -> Bool
prop_reflexivity _ = True

prop_rightExtension :: (a :∈ '[a, b]) => R.SSEXPTYPE a -> R.SSEXPTYPE b -> Bool
prop_rightExtension _ _ = True

prop_leftExtension :: (a :∈ '[b, a]) => R.SSEXPTYPE a -> R.SSEXPTYPE b -> Bool
prop_leftExtension _ _ = True

prop_rightAssociative :: (a :∈ '[a, b, c]) => R.SSEXPTYPE a -> R.SSEXPTYPE b -> R.SSEXPTYPE c -> Bool
prop_rightAssociative _ _ _ = True

prop_reverse :: (a :∈ '[c, b, a]) => R.SSEXPTYPE a -> R.SSEXPTYPE b -> R.SSEXPTYPE c -> Bool
prop_reverse _ _ _ = True

tests :: TestTree
tests = testGroup "Constraints"
    [ testCase "reflexivity"         $ guard $ prop_reflexivity a
    , testCase "right extension"     $ guard $ prop_rightExtension a b
    , testCase "left extension"      $ guard $ prop_leftExtension a b
    , testCase "right associativity" $ guard $ prop_rightAssociative a b c
    , testCase "reverse"             $ guard $ prop_reverse a b c
    ]
  where
    a = R.SInt
    b = R.SReal
    c = R.SLogical
