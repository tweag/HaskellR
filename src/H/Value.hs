-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Unityped representation of the R values in the
-- Haskell runtime,

module H.Value
  where

import qualified Data.Vector.Unboxed as U
import Data.List ( intercalate )
import Data.Function ( on )
import Debug.Trace

-- | Compile time pointer to function, this type will be changed later
type RFunction = String

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
            | RLang RFunction [RValue]        -- ^ Function call
            | RList [RValue]                  -- ^ List type
            | RVar  String                    -- ^ Variable symbol
            deriving (Eq, Show)

-- | Type for the RExpressions that shows how expressions are 
-- presented in source file
--
-- On this step it's imporant to distinguish between REAssign and
-- REFun that is a special version of REAssign
data RExpr = REConst RValue                   -- ^ Constant value
           | REAssign RValue RValue           -- ^ Assign A <- B
           | RECall RFunction [RValue]        -- ^ function call
           | REFun  RValue RValue             -- ^ Function declaration
           deriving (Eq, Show)


-- | Runtime Doubles
newtype RTDouble = RTDouble (U.Vector Double) deriving (Eq)

instance Show RTDouble where
  -- XXX: highly inefficient
  show (RTDouble x) = "[1] " ++ intercalate " " (map show (U.toList x))

mkRTDouble :: [Double] -> RTDouble
mkRTDouble = RTDouble . U.fromList

instance Num RTDouble where
  (+) = liftFunU (+)
  (-) = liftFunU (-)
  (*) = liftFunU (*)
  negate (RTDouble x) = RTDouble $ U.map negate x
  abs    (RTDouble x) = RTDouble $ U.map abs x
  signum (RTDouble x) = RTDouble $ U.map signum x
  fromInteger i = RTDouble $ U.singleton (fromInteger i)

instance Fractional RTDouble where
  (/) = liftFunU (/)
  fromRational x = RTDouble $ U.singleton (fromRational x)

rtdSem :: (Double -> Double -> Double) -> RTDouble -> RTDouble -> RTDouble
rtdSem f (RTDouble x) (RTDouble y)
  | U.length x == 1 = RTDouble $ U.map (f (U.unsafeHead x)) y
  | U.length y == 1 = RTDouble $ U.map (flip f (U.unsafeHead y)) x
  | U.length x == U.length y = RTDouble $ U.zipWith f x y    -- XXX: should check a multipleness
  | otherwise = error "longer object length is not a multiple of shorter object length"

-- | XXX: It's possible to generalize this function:
-- 1. use generic inner types
-- 2. use generic vector types
liftFunU :: (Double -> Double -> Double) -> RTDouble -> RTDouble -> RTDouble
liftFunU f (RTDouble x) (RTDouble y) | trace (show x++" "++show y) False = undefined
liftFunU f (RTDouble x) (RTDouble y) =
    case (compare `on` U.length) x y of
        EQ -> RTDouble $ U.zipWith f x y
        LT -> let (k,z) = U.length y `divMod` U.length x
              in if z == 0 
                   then RTDouble $ U.zipWith f (cycleN k x) y
                   else error "longer object length is not a multiple of shorter object length1"
        GT -> let (k,z) = U.length x `divMod` U.length y
              in if z == 0 
                   then RTDouble $ U.zipWith f x (cycleN k y)
                   else error "longer object length is not a multiple of shorter object length2"
  where
    cycleN i x = U.concat (replicate i x)

