-- |
-- Copyright: (C) 2013 Amgen, Inc.
--            (C) 2015 Tweag I/O Limited.
--
-- Tests for the "Data.Vector.SEXP" module.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Vector where

import Data.AEq
import Data.Int
import Data.Singletons
import qualified Data.Vector.SEXP
import qualified Data.Vector.SEXP as V
import qualified Data.Vector.SEXP.Mutable as VM
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle as S
#else
import qualified Data.Vector.Fusion.Stream as S
#endif
import qualified Foreign.R as R
import H.Prelude
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Assertions

instance (Arbitrary a, V.SVECTOR ty a) => Arbitrary (V.Vector ty a) where
  arbitrary = fmap (\x -> V.fromListN (length x) x) arbitrary

#if MIN_VERSION_vector(0,11,0)
instance Arbitrary a => Arbitrary (S.Bundle v a) where
  arbitrary = fmap (\x -> S.fromListN (length x) x) arbitrary
#else
instance Arbitrary a => Arbitrary (S.Stream a) where
  arbitrary = fmap (\x -> S.fromListN (length x) x) arbitrary
#endif

instance (AEq a, V.SVECTOR ty a) => AEq (V.Vector ty a) where
  a ~== b   = all (uncurry (~==)) $ zip (V.toList a) (V.toList b)

testIdentity :: (Eq a, Show a, Arbitrary a, V.SVECTOR ty a, AEq a) => V.Vector ty a -> TestTree
testIdentity dummy = testGroup "Test identities"
    [ testProperty "fromList.toList == id" (prop_fromList_toList dummy)
    , testProperty "toList.fromList == id" (prop_toList_fromList dummy)
--    , testProperty "unstream.stream == id" (prop_unstream_stream dummy)
--    , testProperty "stream.unstream == id" (prop_stream_unstream dummy)
    ]
  where
    prop_fromList_toList (_:: V.Vector ty a) (v :: V.Vector ty a)
      = (V.fromList . V.toList) v ?~== v
    prop_toList_fromList (_ :: V.Vector ty a) (l :: [a])
      = ((V.toList :: V.Vector ty a -> [a]) . V.fromList) l ?~== l
--    prop_unstream_stream (_ :: V.Vector s ty a) (v :: V.Vector s ty a)
--      = (G.unstream . G.stream) v ?~== v
--    prop_stream_unstream (_ :: V.Vector ty a) (s :: S.Stream a)
--      = ((G.stream :: V.Vector ty a -> S.Stream a) . G.unstream) s == s


testPolymorphicFunctions :: (Eq a, Show a, Arbitrary a, V.SVECTOR ty a, AEq a) => V.Vector ty a -> TestTree
testPolymorphicFunctions dummy = testGroup "Polymorphic functions."
    [ -- Length information
      testProperty "prop_length" (prop_length dummy)
    , testProperty "prop_null"   (prop_null dummy)
    , testProperty "prop_index"  (prop_index dummy)
    , testProperty "prop_head"   (prop_head dummy)
    , testProperty "prop_last"   (prop_last dummy)
    ]
  where
    prop_length (_:: V.Vector ty a) (v :: V.Vector ty a)
      = (length . V.toList) v ~==? V.length v
    prop_null (_:: V.Vector ty a) (v :: V.Vector ty a)
      = (null . V.toList) v ~==? V.null v
    prop_index (_:: V.Vector ty a) (v :: V.Vector ty a, j::Int)
      | V.length v == 0 = True
      | otherwise       = let i = j `mod` V.length v in ((\w -> w !! i) . V.toList) v == (v V.! i)
    prop_head (_:: V.Vector ty a) (v :: V.Vector ty a)
      | V.length v == 0 = True
      | otherwise = (head . V.toList) v == V.head v
    prop_last (_:: V.Vector ty a) (v :: V.Vector ty a)
      | V.length v == 0 = True
      | otherwise = (last . V.toList) v == V.last v

testGeneralSEXPVector :: (Eq a, Show a, Arbitrary a, V.SVECTOR ty a, AEq a) => V.Vector ty a -> TestTree
testGeneralSEXPVector dummy = testGroup "General Vector"
  [ testIdentity dummy
  , testPolymorphicFunctions dummy
  ]

testNumericSEXPVector :: (Eq a, Show a, Arbitrary a, V.SVECTOR ty a, AEq a) => V.Vector ty a -> TestTree
testNumericSEXPVector dummy = testGroup "Test Numeric Vector"
  [ testGeneralSEXPVector dummy
  ]

fromListLength :: TestTree
fromListLength = testCase "fromList should have correct length" $ runRegion $ do
    let lst = [-1.9, -0.1, -2.9]
    let vn = idVec $ V.fromListN 3 lst
    let v  = idVec $ V.fromList lst
    io $ assertEqual "Length should be equal to list length" 3 (V.length vn)
    io $ assertEqual "Length should be equal to list length" 3 (V.length v)
    io $ assertBool "Vectors should be almost equal" (vn ~== v)
    io $ assertEqual "i0" (lst !!0) =<< (V.unsafeIndexM vn 0)
    io $ assertEqual "i1" (lst !!1) =<< (V.unsafeIndexM vn 1)
    io $ assertEqual "i2" (lst !!2) =<< (V.unsafeIndexM vn 2)
    io $ assertEqual "Convertion back to list works" lst (V.toList vn)
    io $ assertEqual "Convertion back to list works 2" lst (V.toList v)
    return ()
  where
    idVec :: V.Vector 'R.Real Double -> V.Vector 'R.Real Double
    idVec = id

vectorIsImmutable :: TestTree
vectorIsImmutable = testCase "immutable vector, should not be affected by SEXP changes" $ do
    i <- runRegion $ do
           s <- fmap (R.cast (sing :: R.SSEXPTYPE 'R.Real)) [r| c(1.0,2.0,3.0) |]
           !mutV <- return $ VM.fromSEXP s
           !immV <- return $ V.fromSEXP s
           VM.unsafeWrite mutV 0 (7::Double)
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
      [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector 'R.Int Int32)]
  , testGroup "Data.Vector.Storable.Vector (Double)"
      [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector 'R.Real Double)]
  , testGroup "Regression tests" [fromListLength
                                 ,vectorIsImmutable
                                 ,vectorCopy
                                 ]
  ]
