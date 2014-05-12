{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# GHC_OPTIONS -fno-warn-orphans #-}
module Main
  where

import qualified Data.Vector.SEXP
import qualified Data.Vector.SEXP as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as S

import Foreign.R ( SEXP )
import qualified Foreign.R as R
import Foreign.R.Type ( IsVector )
import Data.Singletons (SingI)
import Foreign.Storable
import qualified Language.R.Instance as R
    ( initialize
    , defaultConfig )

import Data.Int
import Data.AEq

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Assertions 

main = do
  _ <- R.initialize R.defaultConfig
  defaultMain tests

instance (Arbitrary a, Storable a, IsVector ty, SingI ty, V.ElemRep ty ~ a) => Arbitrary (V.Vector ty a) where
    arbitrary = fmap V.fromList arbitrary

instance Arbitrary a => Arbitrary (S.Stream a) where
    arbitrary = fmap S.fromList arbitrary

instance (AEq a, Storable a, IsVector ty, SingI ty, V.ElemRep ty ~ a) => AEq (V.Vector ty a) where
    a ~== b   = all (uncurry (~==)) $ zip (V.toList a) (V.toList b)

testSanity :: (Eq a, Show a, Arbitrary a, V.SexpVector ty a, AEq a) => V.Vector ty a -> TestTree
testSanity dummy = testGroup "Test sanity"
    [ testProperty "fromList.toList == id" (prop_fromList_toList dummy)
    , testProperty "toList.fromList == id" (prop_toList_fromList dummy)
    , testProperty "unstream.stream == id" (prop_unstream_stream dummy)
--    , testProperty "stream.unstream == id" (prop_stream_unstream dummy)
    ]
  where
    prop_fromList_toList (_:: V.Vector ty a) (v :: V.Vector ty a)
      = (V.fromList . V.toList) v ?~== v
    prop_toList_fromList (_ :: V.Vector ty a) (l :: [a])
      = ((V.toList :: V.Vector ty a -> [a]) . V.fromList) l ?~== l
    prop_unstream_stream (_ :: V.Vector ty a) (v :: V.Vector ty a)
      = (G.unstream . G.stream) v ?~== v
--    prop_stream_unstream (_ :: V.Vector ty a) (s :: S.Stream a)
--      = ((G.stream :: V.Vector ty a -> S.Stream a) . G.unstream) s == s


testPolymorphicFunctions :: (Eq a, Show a, Arbitrary a, V.SexpVector ty a, AEq a) => V.Vector ty a -> TestTree
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

testGeneralSEXPVector dummy = testGroup "General Vector"
  [ testSanity dummy
  , testPolymorphicFunctions dummy
  ]


testNumericSEXPVector dummy = testGroup "Test Numeric Vector"
  [ testGeneralSEXPVector dummy
  ]

tests = testGroup "Tests."
  [ testGroup "Data.Vector.Storable.Vector (Int32)"  [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector 'R.Int Int32)]
  , testGroup "Data.Vector.Storable.Vector (Double)" [testNumericSEXPVector (undefined :: Data.Vector.SEXP.Vector 'R.Real Double)]
  ]

