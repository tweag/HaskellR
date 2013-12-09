-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language GADTs #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}
module H.Internal.Literal
  ( Literal(..)
  , HFunWrap(..)
  , funToSEXP
    -- * wrapper helpers
  ) where

import           H.HExp as HExp
import           H.Internal.FunWrappers
import           H.Internal.TH
import qualified Data.Vector.SEXP as SVector
import qualified Foreign.R as R

import qualified Data.Vector.Storable as V

import Control.Applicative ((<$>))
import Control.Monad ( zipWithM_ )
import Data.Int (Int32)
import Data.Complex (Complex)
import Foreign          ( FunPtr, castFunPtr, castPtr )
import Foreign.C.String ( newCString )
import Foreign.Storable ( Storable, pokeElemOff )
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'R.SEXP'.
class Literal a b | a -> b where
    mkSEXP :: a -> R.SEXP b
    fromSEXP :: R.SEXP b -> a

mkSEXPVector :: Storable a
             => R.SEXPTYPE
             -> [a]
             -> R.SEXP (R.Vector a)
mkSEXPVector ty xs = unsafePerformIO $ do
    vec <- R.allocVector ty $ length xs
    ptr <- castPtr <$> R.vector (castPtr vec)
    zipWithM_ (pokeElemOff ptr) [0..] xs
    return vec

instance Literal [Bool] (R.Vector Bool) where
    mkSEXP = mkSEXPVector R.Logical
    fromSEXP (hexp -> Logical (SVector.Vector v)) = V.toList v
    fromSEXP _ = error "[Logical] expected where some other expression appeared."

instance Literal [Int32] (R.Vector Int32) where
    mkSEXP = mkSEXPVector R.Int
    fromSEXP (hexp -> Int (SVector.Vector v)) = V.toList v
    fromSEXP _ = error "[Int32] expected where some other expression appeared."

instance Literal [Double] (R.Vector Double) where
    mkSEXP = mkSEXPVector R.Real
    fromSEXP (hexp -> Real (SVector.Vector v)) = V.toList v
    fromSEXP _ = error "[Double] expected where some other expression appeared."

instance Literal [Complex Double] (R.Vector (Complex Double)) where
    mkSEXP = mkSEXPVector R.Complex
    fromSEXP (hexp -> Complex (SVector.Vector v)) = V.toList v
    fromSEXP _ = error "[Complex Double] expected where some other expression appeared."

-- | Named after eponymous "GHC.Exts" function.
the :: Literal [a] (R.Vector a) => R.SEXP (R.Vector a) -> a
the (fromSEXP -> xs)
  | length xs == 1 = head xs
  | otherwise = error "Not a singleton vector."

instance Literal Bool (R.Vector Bool) where
    mkSEXP x = mkSEXP [x]
    fromSEXP = the

instance Literal Int32 (R.Vector Int32) where
    mkSEXP x = mkSEXP [x]
    fromSEXP = the

instance Literal Double (R.Vector Double) where
    mkSEXP x = mkSEXP [x]
    fromSEXP = the

instance Literal (Complex Double) (R.Vector (Complex Double)) where
    mkSEXP x = mkSEXP [x]
    fromSEXP = the

instance Literal (R.SEXP a) b where
    mkSEXP = R.sexp . R.unsexp
    fromSEXP = R.sexp . R.unsexp

instance Literal String (R.String) where
    mkSEXP x = unsafePerformIO $ R.mkString =<< newCString x
    fromSEXP  = error "Unimplemented. fromSEXP (String)"

instance Literal a b => Literal (IO a) R.ExtPtr where
    mkSEXP = funToSEXP wrap0
    fromSEXP = error "Unimplemented. fromSEXP (IO a)"

instance (Literal a a0, Literal b b0) => Literal (a -> IO b) R.ExtPtr where
    mkSEXP = funToSEXP wrap1
    fromSEXP = error "Unimplemented. fromSEXP (a -> IO b)"

instance (Literal a a0, Literal b b0, Literal c c0)
         => Literal (a -> b -> IO c) R.ExtPtr where
    mkSEXP = funToSEXP wrap2
    fromSEXP = error "Unimplemented. fromSEXP (a -> b -> IO c)"

class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (IO a) (IO (R.SEXP la)) where
    hFunWrap a = fmap mkSEXP a

-- | A class for functions that can be converted to functions on SEXPs.
instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP la -> wb) where
    hFunWrap f a = hFunWrap $ f $ fromSEXP a

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr () -> IO (R.SEXP R.Any)


funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> R.SEXP c
funToSEXP w x = unsafePerformIO $ fmap castPtr . funPtrToSEXP . castFunPtr
                =<< w (hFunWrap x)

$(thWrapperLiterals 4 25)
