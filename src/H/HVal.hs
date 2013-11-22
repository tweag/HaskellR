-- |
-- Copyright: 2013 (C) Amgen, Inc
{-# Language ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE UndecidableInstances #-}
module H.HVal
  ( HVal
  , Literal(..)
  -- * Conversion
  , fromHVal
  , safeFromHVal
  , someHVal
  , toHVal
  -- * Arithmetics
  , rplus
  , rminus
  , rmult
  , rfrac
  ) where

import H.HExp
import qualified Language.R as R
import qualified Foreign.R  as R

import Control.Applicative
import Control.Monad ( forM_ )
import Foreign ( castPtr, castFunPtr, FunPtr, newForeignPtr_, pokeElemOff )
import Foreign.C.String ( newCString )
import System.IO.Unsafe ( unsafePerformIO )

-- Temporary
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.Storable as V
import Data.List ( intercalate )
import Data.Word

-- | Runtime universe of R Values
data HVal = forall a . SEXP (R.SEXP a)
          | HLam2 (HVal -> HVal)

instance Show HVal where
    show (SEXP s)  = unsafePerformIO $ do
      let s' = castPtr s :: R.SEXP (R.Vector Double)
      l <- R.length s'
      v <- flip V.unsafeFromForeignPtr0 l <$> (newForeignPtr_ =<< R.real s')
      return $ "[1] " ++ (intercalate " " (map show $ V.toList v))
    show (HLam2 _) = "HLam2 {..}"

-- | Project from HVal to R SEXP.
--
-- Note that this function is partial.
fromHVal :: HVal -> R.SomeSEXP
fromHVal (SEXP x) = R.SomeSEXP x
fromHVal _        = error "toSEXP: not an SEXP"

-- | Safe version of 'toSEXP'.
safeFromHVal :: HVal -> Maybe (R.SomeSEXP)
safeFromHVal (SEXP x) = Just (R.SomeSEXP x)
safeFromHVal _        = Nothing

someHVal :: R.SomeSEXP -> HVal
someHVal (R.SomeSEXP x) = SEXP x

toHVal :: R.SEXP a -> HVal
toHVal x = SEXP x

--------------------------------------------------------------------------------
-- Arithmetic subset of H                                                     --
--------------------------------------------------------------------------------
{-
instance Num HVal where
    fromInteger x = SEXP $ mkSEXP (fromInteger x :: Double)
    a + b = someHVal (rplus  (fromHVal a) (fromHVal b))
    a - b = someHVal (rminus (fromHVal a) (fromHVal b))
    a * b = someHVal (rmult  (fromHVal a) (fromHVal b))
    abs _ = error "unimplemented."
    signum _ = error "unimplemented."

instance Fractional HVal where
    fromRational x = SEXP (mkSEXP (fromRational x :: Double))
    a / b = SEXP (rfrac (fromHVal a) (fromHVal b))
-}

rplus, rminus, rmult, rfrac :: R.SEXP a -> R.SEXP a -> R.SEXP a
rplus  x y = R.r2 "+" x y
rminus x y = R.r2 "-" x y
rmult  x y = R.r2 "*" x y
rfrac  x y = R.r2 "/" x y


-- | Represents a value that can be converted into S Expression
class Literal a b | a -> b where
    mkSEXP :: a -> R.SEXP b
    fromSEXP :: R.SEXP b -> a

instance Literal Double (R.Vector Double) where
    mkSEXP x = unsafePerformIO $ do
      v  <- R.allocVector R.Real 1
      pt <- R.real v
      pokeElemOff pt 0 (fromRational . toRational $ x)
      return v

    fromSEXP s =
      case hexp s of
        Real (SVector.Vector v) | V.length v == 1 -> v V.! 0
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
        _ -> error "[Double] expected where some other expression appeared."

instance Literal String (R.Vector Word8) where
    mkSEXP x = unsafePerformIO $ R.mkString =<< newCString x

    fromSEXP s =
      case hexp s of
        Char (SVector.Vector v) -> map (toEnum . fromEnum) $  V.toList v
        _ -> error "String expected where some other expression appeared."


--------------------------------------------------------------------------------
-- Helpers for passing functions pointers between Haskell and R               --
--------------------------------------------------------------------------------

-- | A class for functions that can be converted to functions on SEXPs.
class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (IO a) (IO (R.SEXP la)) where
    hFunWrap a = fmap mkSEXP a

instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP la -> wb) where
    hFunWrap f a = hFunWrap $ f $ fromSEXP a

foreign import ccall "wrapper" wrap0 :: IO (R.SEXP a) -> IO (FunPtr (IO (R.SEXP a)))

foreign import ccall "wrapper" wrap1
    :: (R.SEXP a -> IO (R.SEXP b)) -> IO (FunPtr (R.SEXP a -> IO (R.SEXP b)))

foreign import ccall "wrapper" wrap2
    :: (R.SEXP a -> R.SEXP b -> IO (R.SEXP c))
    -> IO (FunPtr (R.SEXP a -> R.SEXP b -> IO (R.SEXP c)))

foreign import ccall "wrapper" wrap3
    :: (R.SEXP a -> R.SEXP b -> R.SEXP c -> IO (R.SEXP d))
    -> IO (FunPtr (R.SEXP a -> R.SEXP b -> R.SEXP c -> IO (R.SEXP d)))

foreign import ccall "wrapper" wrap4
    :: (R.SEXP a -> R.SEXP b -> R.SEXP c -> R.SEXP d -> IO (R.SEXP e))
    -> IO (FunPtr (R.SEXP a -> R.SEXP b -> R.SEXP c -> R.SEXP d -> IO (R.SEXP e)))

foreign import ccall "wrapper" wrap5
    :: (  R.SEXP a -> R.SEXP b -> R.SEXP c
       -> R.SEXP d -> R.SEXP e -> IO (R.SEXP f)
       )
    -> IO (FunPtr (  R.SEXP a -> R.SEXP b -> R.SEXP c
                  -> R.SEXP d -> R.SEXP e -> IO (R.SEXP f)
                  )
          )

foreign import ccall "wrapper" wrap6
    :: (  R.SEXP a -> R.SEXP b -> R.SEXP c
       -> R.SEXP d -> R.SEXP e -> R.SEXP f -> IO (R.SEXP g)
       )
    -> IO (FunPtr (  R.SEXP a -> R.SEXP b -> R.SEXP c
                  -> R.SEXP d -> R.SEXP e -> R.SEXP f -> IO (R.SEXP g)
                  )
          )

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr () -> IO (R.SEXP R.Any)

funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> R.SEXP c
funToSEXP w x = unsafePerformIO $ fmap castPtr . funPtrToSEXP . castFunPtr
                =<< w (hFunWrap x)

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

instance (Literal a a0, Literal b b0, Literal c c0, Literal d d0)
         => Literal (a -> b -> c -> IO d) R.ExtPtr where
    mkSEXP = funToSEXP wrap3
    fromSEXP = error "Unimplemented. fromSEXP (a -> b -> c -> IO d)"

instance (Literal a a0, Literal b b0, Literal c c0, Literal d d0, Literal e e0)
         => Literal (a -> b -> c -> d -> IO e) R.ExtPtr where
    mkSEXP = funToSEXP wrap4
    fromSEXP = error "Unimplemented. fromSEXP (a -> b -> c -> d -> IO e)"

instance ( Literal a a0, Literal b b0, Literal c c0, Literal d d0, Literal e e0
         , Literal f f0
         )
         => Literal (a -> b -> c -> d -> e -> IO f) R.ExtPtr where
    mkSEXP = funToSEXP wrap5
    fromSEXP = error "Unimplemented. fromSEXP (a -> b -> c -> d -> e -> IO f)"

instance ( Literal a a0, Literal b b0, Literal c c0, Literal d d0, Literal e e0
         , Literal f f0, Literal g g0
         )
         => Literal (a -> b -> c -> d -> e -> f -> IO g) R.ExtPtr where
    mkSEXP = funToSEXP wrap6
    fromSEXP = error "Unimplemented. fromSEXP (a -> b -> c -> d -> e -> f -> IO g)"
