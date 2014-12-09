{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
  where

import qualified Data.Vector.SEXP
import qualified Data.Vector.SEXP as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as S

import H.Prelude hiding (Show)
import qualified Foreign.R as R
import qualified Language.R.Instance as R
    ( defaultConfig )

import Data.Int
import Data.AEq

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Assertions

main :: IO ()
main = withEmbeddedR R.defaultConfig $
  defaultMain tests

instance (Arbitrary a, V.VECTOR s ty a) => Arbitrary (V.Vector s ty a) where
    arbitrary = fmap V.fromList arbitrary

instance Arbitrary a => Arbitrary (S.Stream a) where
    arbitrary = fmap S.fromList arbitrary

instance (AEq a, V.VECTOR s ty a) => AEq (V.Vector s ty a) where
    a ~== b   = all (uncurry (~==)) $ zip (V.toList a) (V.toList b)

testSanity :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testSanity dummy = testGroup "Test sanity"
    [ testProperty "fromList.toList == id" (prop_fromList_toList dummy)
    , testProperty "toList.fromList == id" (prop_toList_fromList dummy)
    , testProperty "unstream.stream == id" (prop_unstream_stream dummy)
--    , testProperty "stream.unstream == id" (prop_stream_unstream dummy)
    ]
  where
    prop_fromList_toList (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      = (V.fromList . V.toList) v ?~== v
    prop_toList_fromList (_ :: V.Vector s ty a) (l :: [a])
      = ((V.toList :: V.Vector s ty a -> [a]) . V.fromList) l ?~== l
    prop_unstream_stream (_ :: V.Vector s ty a) (v :: V.Vector s ty a)
      = (G.unstream . G.stream) v ?~== v
--    prop_stream_unstream (_ :: V.Vector ty a) (s :: S.Stream a)
--      = ((G.stream :: V.Vector ty a -> S.Stream a) . G.unstream) s == s


testPolymorphicFunctions :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testPolymorphicFunctions dummy = testGroup "Polymorphic functions."
    [ -- Length information
      testProperty "prop_length" (prop_length dummy)
    , testProperty "prop_null"   (prop_null dummy)
    , testProperty "prop_index"  (prop_index dummy)
    , testProperty "prop_head"   (prop_head dummy)
    , testProperty "prop_last"   (prop_last dummy)
    ]
  where
    prop_length (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      = (length . V.toList) v ~==? V.length v
    prop_null (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      = (null . V.toList) v ~==? V.null v
    prop_index (_:: V.Vector s ty a) (v :: V.Vector s ty a, j::Int)
      | V.length v == 0 = True
      | otherwise       = let i = j `mod` V.length v in ((\w -> w !! i) . V.toList) v == (v V.! i)
    prop_head (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      | V.length v == 0 = True
      | otherwise = (head . V.toList) v == V.head v
    prop_last (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      | V.length v == 0 = True
      | otherwise = (last . V.toList) v == V.last v

testGeneralSEXPVector :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testGeneralSEXPVector dummy = testGroup "General Vector"
  [ testSanity dummy
  , testPolymorphicFunctions dummy
  ]


testNumericSEXPVector :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testNumericSEXPVector dummy = testGroup "Test Numeric Vector"
  [ testGeneralSEXPVector dummy
  ]

tests :: TestTree
tests = testGroup "Tests."
  [ testGroup "Data.Vector.Storable.Vector (Int32)"  [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector s 'R.Int Int32)]
  , testGroup "Data.Vector.Storable.Vector (Double)" [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector s 'R.Real Double)]
  , testGroup "Regression tests" [fromListLength]
  ]

idVec :: V.Vector s R.Real Double -> V.Vector s R.Real Double
idVec = id

fromListLength :: TestTree
fromListLength = testCase "fromList should have correct length" $ runRegion $ do
   _ <- io $ return $ idVec $ V.fromListN 3 [-1.9,-0.1,-2.9]
   let v = idVec $ V.fromList [-1.9,-0.1,-2.9]
   _ <- io $ R.protect (V.unVector v)
   io $ assertEqual "Length should be equal to list length" 3 (V.length v)
   -- io $ Prelude.print v
   return ()
