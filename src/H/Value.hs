-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Unityped representation of the R values in the
-- Haskell runtime,

module H.Value
  where

import qualified Data.Vector.Unboxed as U

-- | Description of the RValues.
--
-- This is Haskell side of representation thus Haskell Runtime
-- manages such values, as a result such values have to be 
-- encoded to R values before processing in R runtime.
--
-- Such representation is used at least on translation stage.
--
data RValue = RNil                            -- ^ Nil value
            | RReal (U.Vector Double)         -- ^ Vector of Real variables
            | RLang String [RValue]           -- ^ Function call
            | RVar  String                    -- ^ Variable symbol
            deriving (Eq, Show)

-- | Runtime Doubles
newtype RTDouble = RTDouble (U.Vector Double) deriving (Eq,Show)

mkRTDouble :: [Double] -> RTDouble
mkRTDouble = RTDouble . U.fromList

instance Num RTDouble where
  (+) = rtdSem (+)
  (-) = rtdSem (-)
  (*) = rtdSem (*)
  negate (RTDouble x) = RTDouble $ U.map negate x
  abs    (RTDouble x) = RTDouble $ U.map abs x
  signum (RTDouble x) = RTDouble $ U.map signum x
  fromInteger i = RTDouble $ U.singleton (fromInteger i)

instance Fractional RTDouble where
  (/) = rtdSem (/)
  fromRational x = RTDouble $ U.singleton (fromRational x)

rtdSem :: (Double -> Double -> Double) -> RTDouble -> RTDouble -> RTDouble
rtdSem f (RTDouble x) (RTDouble y)
  | U.length x == 1 = RTDouble $ U.map (f (U.unsafeHead x)) y
  | U.length y == 1 = RTDouble $ U.map (flip f (U.unsafeHead y)) x
  | U.length x == U.length y = RTDouble $ U.zipWith f x y    -- XXX: should check a multipleness
  | otherwise = error "longer object length is not a multiple of shorter object length"


