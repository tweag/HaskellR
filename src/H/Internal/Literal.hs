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
import Control.Monad ( forM_ )
import Foreign          ( FunPtr, castFunPtr, castPtr )
import Foreign.C.String ( newCString )
import Foreign.Storable ( pokeElemOff )
import System.IO.Unsafe ( unsafePerformIO )

-- | Represents a value that can be converted into S Expression
class Literal a b | a -> b where
    mkSEXP :: a -> R.SEXP b
    fromSEXP :: R.SEXP c -> a

instance Literal Double (R.Vector Double) where
    mkSEXP x = unsafePerformIO $ do
      v  <- R.allocVector R.Real 1
      pt <- R.real v
      pokeElemOff pt 0 (fromRational . toRational $ x)
      return v

    fromSEXP s =
      case hexp s of
        Real (SVector.Vector v) | V.length v == 1 -> v V.! 0
        Int  (SVector.Vector v) | V.length v == 1 -> fromIntegral (v V.! 0)
        _ -> error "Double expected where some other expression appeared."

instance Literal [Double] (R.Vector Double) where
    mkSEXP x = unsafePerformIO $ do
        v  <- R.allocVector R.Real l
        pt <- R.real v
        forM_ (zip x [0..]) $ \(g,i) -> do
          pokeElemOff pt i g
        return v
      where
        l = length x
    fromSEXP s =
      case hexp s of
        Real (SVector.Vector v) -> V.toList v
        Int  (SVector.Vector v) -> map fromIntegral $ V.toList v
        _ -> error "[Double] expected where some other expression appeared."

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
