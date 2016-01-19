-- |
-- Copyright: (C) 2013 Amgen, Inc.
--            (C) 2015 Tweag I/O Limited.
--
-- Tests for the "Data.Vector.SEXP" module.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Vector where

import Data.AEq
import Data.Int
import Data.Singletons
import qualified Data.Vector.SEXP
import qualified Data.Vector.SEXP as V
import qualified Data.Vector.SEXP.Mutable as VM
-- import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as S
import qualified Foreign.R as R
import H.Prelude hiding (Show)
import Language.R.QQ
import Language.R.HExp (HExp(..), hexp)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Assertions

instance (Arbitrary a, V.VECTOR s ty a) => Arbitrary (V.Vector s ty a) where
  arbitrary = fmap V.fromList arbitrary

instance Arbitrary a => Arbitrary (S.Stream a) where
  arbitrary = fmap S.fromList arbitrary

instance (AEq a, V.VECTOR s ty a) => AEq (V.Vector s ty a) where
  a ~== b   = all (uncurry (~==)) $ zip (V.toList a) (V.toList b)

testIdentity :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testIdentity dummy = testGroup "Test identities"
    [ testProperty "fromList.toList == id" (prop_fromList_toList dummy)
    , testProperty "toList.fromList == id" (prop_toList_fromList dummy)
--    , testProperty "unstream.stream == id" (prop_unstream_stream dummy)
--    , testProperty "stream.unstream == id" (prop_stream_unstream dummy)
    ]
  where
    prop_fromList_toList (_:: V.Vector s ty a) (v :: V.Vector s ty a)
      = (V.fromList . V.toList) v ?~== v
    prop_toList_fromList (_ :: V.Vector s ty a) (l :: [a])
      = ((V.toList :: V.Vector s ty a -> [a]) . V.fromList) l ?~== l
--    prop_unstream_stream (_ :: V.Vector s ty a) (v :: V.Vector s ty a)
--      = (G.unstream . G.stream) v ?~== v
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
  [ testIdentity dummy
  , testPolymorphicFunctions dummy
  ]

testNumericSEXPVector :: (Eq a, Show a, Arbitrary a, V.VECTOR s ty a, AEq a) => V.Vector s ty a -> TestTree
testNumericSEXPVector dummy = testGroup "Test Numeric Vector"
  [ testGeneralSEXPVector dummy
  ]

fromListLength :: TestTree
fromListLength = testCase "fromList should have correct length" $ runRegion $ do
    _ <- return $ idVec $ V.fromListN 3 [-1.9, -0.1, -2.9]
    let v = idVec $ V.fromList [-1.9, -0.1, -2.9]
    io $ assertEqual "Length should be equal to list length" 3 (V.length v)
    return ()
  where
    idVec :: V.Vector s 'R.Real Double -> V.Vector s 'R.Real Double
    idVec = id

vectorIsImmutable :: TestTree
vectorIsImmutable = testCase "fromList should have correct length" $ do
    i <- runRegion $ do
           s <- fmap (R.cast (sing :: R.SSEXPTYPE 'R.Real)) [r| c(1.0,2.0,3.0) |]
           let mutV = VM.fromSEXP s
           let immV = V.fromSEXP s
           VM.unsafeWrite mutV 0 7
           return $ immV V.! 0
    i @?= 1

vectorCopy :: TestTree
vectorCopy = testCase "Copying vector of doubles works" $ runRegion $ do
  let vs1 = V.toSEXP (V.fromList [1..3::Double]) :: R.SEXP s 'R.Real
      vs2 = V.unsafeToSEXP (V.fromList [1..3::Double]) :: R.SEXP s 'R.Real
  R.SomeSEXP (hexp -> Logical [R.TRUE]) <- [r| identical(vs1_hs, vs2_hs) |]
  return ()

tests :: TestTree
tests = testGroup "Tests."
  [ testGroup "Data.Vector.Storable.Vector (Int32)"
      [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector s 'R.Int Int32)]
  , testGroup "Data.Vector.Storable.Vector (Double)"
      [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector s 'R.Real Double)]
  , testGroup "Regression tests" [fromListLength
                                 ,vectorIsImmutable
                                 ,vectorCopy
                                 ]
  ]
