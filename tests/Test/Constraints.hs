module Test.Constraints
  ( tests )
  where

import H.Constraints
import Control.Monad (guard)

import Test.Tasty
import Test.Tasty.HUnit

data A = A
data B = B
data C = C

prop_reflexivity :: (a :∈ a) => a -> Bool
prop_reflexivity _ = True

prop_rightExtension :: (a :∈ a :+: b) => a -> b -> Bool
prop_rightExtension _ _ = True

prop_leftExtension :: (a :∈ b :+: a) => a -> b -> Bool
prop_leftExtension _ _ = True

prop_rightAssociative :: (a :∈ a :+: b :+: c) => a -> b -> c -> Bool
prop_rightAssociative _ _ _ = True

prop_reverse :: (a :∈ c :+: b :+: a) => a -> b -> c -> Bool
prop_reverse _ _ _ = True

tests :: TestTree
tests = testGroup "Constraints"
    [ testCase "reflexivity"         $ guard $ prop_reflexivity A
    , testCase "right extension"     $ guard $ prop_rightExtension A B
    , testCase "left extension"      $ guard $ prop_leftExtension A B
    , testCase "right associativity" $ guard $ prop_rightAssociative A B C
    , testCase "reverse"             $ guard $ prop_reverse A B C
    ]
