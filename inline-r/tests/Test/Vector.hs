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
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--ple" @-}

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
import Foreign.Storable
import qualified Foreign.R as R
import qualified Foreign.R.Internal as R (checkSEXPTYPE)
import H.Prelude
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Assertions

import qualified Control.Memory.Region
import qualified Foreign.R.Type
import qualified Foreign.R.Internal
import qualified Control.Monad.Reader
import qualified Data.IORef
import qualified Data.Word
import qualified Data.Vector.SEXP
import qualified Foreign.C.String
import qualified GHC.ForeignPtr -- Needed to help LH name resolution
import qualified GHC.ST -- Needed to help LH name resolution


#if MIN_VERSION_vector(0,11,0)
instance Arbitrary a => Arbitrary (S.Bundle v a) where
  arbitrary = fmap (\x -> S.fromListN (length x) x) arbitrary
#else
instance Arbitrary a => Arbitrary (S.Stream a) where
  arbitrary = fmap (\x -> S.fromListN (length x) x) arbitrary
#endif

instance (AEq a, Storable a) => AEq (V.Vector a) where
  a ~== b   = all (uncurry (~==)) $ zip (V.toList a) (V.toList b)

testIdentity :: (Eq a, Show a, Arbitrary a, Storable a, AEq a) => V.VSEXPTYPE s a -> TestTree
testIdentity vt = testGroup "Test identities"
    [ testProperty "fromList.toList == id" (prop_fromList_toList . V.fromList vt)
    , testProperty "toList.fromList == id" (prop_toList_fromList vt)
--    , testProperty "unstream.stream == id" (prop_unstream_stream dummy)
--    , testProperty "stream.unstream == id" (prop_stream_unstream dummy)
    ]
  where
    prop_fromList_toList v
      = (V.fromList vt . V.toList) v ?~== v
    prop_toList_fromList vt l
      = (V.toList . V.fromList vt) l ?~== l
--    prop_unstream_stream (_ :: V.Vector s ty a) (v :: V.Vector s ty a)
--      = (G.unstream . G.stream) v ?~== v
--    prop_stream_unstream (_ :: V.Vector ty a) (s :: S.Stream a)
--      = ((G.stream :: V.Vector ty a -> S.Stream a) . G.unstream) s == s


-- XXX: LH wants to check properties of head, last, and (!!)
{-@ ignore testPolymorphicFunctions @-}
testPolymorphicFunctions :: (Eq a, Show a, Arbitrary a, Storable a, AEq a) => V.VSEXPTYPE s a -> TestTree
testPolymorphicFunctions vt = testGroup "Polymorphic functions."
    [ -- Length information
      testProperty "prop_length" (prop_length . V.fromList vt)
    , testProperty "prop_null"   (prop_null . V.fromList vt)
    , testProperty "prop_index"  (prop_index . V.fromList vt)
    , testProperty "prop_head"   (prop_head . V.fromList vt)
    , testProperty "prop_last"   (prop_last . V.fromList vt)
    ]
  where
    prop_length v
      = (length . V.toList) v ~==? V.length v
    prop_null v
      = (null . V.toList) v ~==? V.null v
    prop_head v
      | V.length v == 0 = True
      | otherwise = (head . V.toList) v == V.head v
    prop_last v
      | V.length v == 0 = True
      | otherwise = (last . V.toList) v == V.last v

-- XXX: LH wants to check properties of (!!)
-- XXX: LH cannot ignore local functions (?) so moved this to the top level
{-@ ignore prop_index @-}
prop_index v (j::Int) =
      let n = V.length v
       in if n == 0 then True
          else let i = j `mod` n in ((\w -> w !! i) . V.toList) v == (v V.! i)

testGeneralSEXPVector :: (Eq a, Show a, Arbitrary a, Storable a, AEq a) => V.VSEXPTYPE s a -> TestTree
testGeneralSEXPVector vt = testGroup "General Vector"
  [ testIdentity vt
  , testPolymorphicFunctions vt
  ]

testNumericSEXPVector :: (Eq a, Show a, Arbitrary a, Storable a, AEq a) => V.VSEXPTYPE s a -> TestTree
testNumericSEXPVector vt = testGroup "Test Numeric Vector"
  [ testGeneralSEXPVector vt
  ]

{-@ ignore fromListLength @-}
fromListLength :: TestTree
fromListLength = testCase "fromList should have correct length" $ runRegion $ do
    let lst = [-1.9, -0.1, -2.9]
    let vn = idVec $ V.fromListN V.VReal 3 lst
    let v  = idVec $ V.fromList V.VReal lst
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
    idVec :: V.Vector Double -> V.Vector Double
    idVec = id

-- XXX: Should pass with ple, but it doesn't
{-@ ignore vectorIsImmutable @-}
vectorIsImmutable :: TestTree
vectorIsImmutable = testCase "immutable vector, should not be affected by SEXP changes" $ do
    i <- runRegion $ do
           s <- fmap (R.checkSEXPTYPE R.Real) [r| c(1.0,2.0,3.0) |]
           !mutV <- return $ VM.fromSEXP s
           !immV <- return $ V.fromSEXP s
           VM.unsafeWrite mutV 0 (7::Double)
           -- XXX: fromSEXP has become unsafe!
           return (immV V.! 0 :: Double)
    i @?= 1

vectorCopy :: TestTree
vectorCopy = testCase "Copying vector of doubles works" $ runRegion $ do
  let vs1 = V.toSEXP (V.fromList V.VReal [1..3::Double]) :: R.SEXP s
      vs2 = V.unsafeToSEXP (V.fromList V.VReal [1..3::Double]) :: R.SEXP s
  -- XXX: Lost ability to use overloaded lists!
  -- Logical [R.True] <- hexp <$> ([r| identical(vs1_hs, vs2_hs) |] :: R s (R.SEXP s))
  Logical (V.toList -> [R.TRUE]) <- hexp <$> ([r| identical(vs1_hs, vs2_hs) |] :: R s (R.SEXP s))
  return ()

tests :: TestTree
tests = testGroup "Tests."
  [ testGroup "Data.Vector.Storable.Vector (Int32)"
      [testNumericSEXPVector V.VInt]
  , testGroup "Data.Vector.Storable.Vector (Double)"
      [testNumericSEXPVector V.VReal]
  , testGroup "Regression tests" [fromListLength
                                 ,vectorIsImmutable
                                 ,vectorCopy
                                 ]
  ]
