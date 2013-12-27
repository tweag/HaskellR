-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# Language FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language GADTs #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Language.R.Literal
  ( Literal(..)
  , mkSEXPVector
  , HFunWrap(..)
  , funToSEXP
  , mkProtectedSEXPVector
    -- * wrapper helpers
  ) where

import           H.Internal.Prelude
import           Language.R.HExp
import           Language.R.Internal.FunWrappers
import           Language.R.Internal.FunWrappers.TH
import           H.Internal.REnv ( REnv(..) )
import qualified Data.Vector.SEXP as SVector
import qualified Foreign.R as R
import           Language.R ( withProtected )

import qualified Data.Vector.Storable as V

import Control.Applicative ((<$>))
import Control.Monad ( void, zipWithM_ )
import Data.Int (Int32)
import Data.Complex (Complex)
import Foreign          ( FunPtr, castFunPtr, castPtr )
import Foreign.C.String ( newCString )
import Foreign.Storable ( Storable, pokeElemOff )
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'SEXP'.
class Literal a b | a -> b where
    mkSEXP :: a -> SEXP b
    fromSEXP :: SEXP c -> a

mkSEXPVector :: Storable a
             => SEXPTYPE
             -> [a]
             -> SEXP (R.Vector a)
mkSEXPVector ty xs = unsafePerformIO $
    withProtected (R.allocVector ty $ length xs) $ \vec -> do
      ptr <- castPtr <$> R.vector (castPtr vec)
      zipWithM_ (pokeElemOff ptr) [0..] xs
      return vec

mkProtectedSEXPVector :: SEXPTYPE
                      -> [SEXP a]
                      -> SEXP (R.Vector b)
mkProtectedSEXPVector ty xs = unsafePerformIO $ do
    mapM_ (void . R.protect) xs
    z <- withProtected (R.allocVector ty $ length xs) $ \vec -> do
           ptr <- castPtr <$> R.vector (castPtr vec)
           zipWithM_ (pokeElemOff ptr) [0..] xs
           return vec
    R.unprotect (length xs)
    return z

instance Literal [R.Logical] (R.Vector R.Logical) where
    mkSEXP = mkSEXPVector R.Logical
    fromSEXP (hexp -> Logical (SVector.Vector v)) = V.toList v
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal [Int32] (R.Vector Int32) where
    mkSEXP = mkSEXPVector R.Int
    fromSEXP (hexp -> Int (SVector.Vector v)) = V.toList v
    fromSEXP (hexp -> Real (SVector.Vector v)) = map round (V.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal [Double] (R.Vector Double) where
    mkSEXP = mkSEXPVector R.Real
    fromSEXP (hexp -> Real (SVector.Vector v)) = V.toList v
    fromSEXP (hexp -> Int (SVector.Vector v)) = map fromIntegral (V.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal [Complex Double] (R.Vector (Complex Double)) where
    mkSEXP = mkSEXPVector R.Complex
    fromSEXP (hexp -> Complex (SVector.Vector v)) = V.toList v
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

-- | Named after eponymous "GHC.Exts" function.
the :: Literal [a] (R.Vector a) => SEXP (R.Vector a) -> a
the (fromSEXP -> xs)
  | length xs == 1 = head xs
  | otherwise = failure "the" "Not a singleton vector."

instance Literal R.Logical (R.Vector R.Logical) where
    mkSEXP x = mkSEXP [x]
    fromSEXP x@(hexp -> Logical{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal Int32 (R.Vector Int32) where
    mkSEXP x = mkSEXP [x]
    fromSEXP x@(hexp -> Int{}) = the x
    fromSEXP x@(hexp -> Real{}) = round (the x)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal Double (R.Vector Double) where
    mkSEXP x = mkSEXP [x]
    fromSEXP x@(hexp -> Real{}) = the x
    fromSEXP x@(hexp -> Int{})  = fromIntegral (the x)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal (Complex Double) (R.Vector (Complex Double)) where
    mkSEXP x = mkSEXP [x]
    fromSEXP x@(hexp -> Complex{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

instance Literal (SEXP a) b where
    mkSEXP   = R.unsafeCoerce
    fromSEXP = R.unsafeCoerce

instance Literal SomeSEXP b where
    mkSEXP s   = R.unSomeSEXP s R.unsafeCoerce
    fromSEXP   = SomeSEXP

instance Literal String (R.String) where
    mkSEXP x = unsafePerformIO $ R.mkString =<< newCString x
    fromSEXP  = unimplemented "Literal String fromSEXP"

instance Literal a b => Literal (R a) R.ExtPtr where
    mkSEXP   = funToSEXP wrap0
    fromSEXP = unimplemented "Literal (Ra a) fromSEXP"

instance (Literal a a0, Literal b b0) => Literal (a -> R b) R.ExtPtr where
    mkSEXP   = funToSEXP wrap1
    fromSEXP = unimplemented "Literal (a -> R b) fromSEXP"

instance (Literal a a0, Literal b b0, Literal c c0)
         => Literal (a -> b -> R c) R.ExtPtr where
    mkSEXP   = funToSEXP wrap2
    fromSEXP = unimplemented "Literal (a -> b -> IO c) fromSEXP"

class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (R a) (IO (SEXP la)) where
    hFunWrap a = fmap (mkSEXP $!) (runR REnv a)

-- | A class for functions that can be converted to functions on SEXPs.
instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (SEXP la -> wb) where
    hFunWrap f a = hFunWrap $ f $ fromSEXP a

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr () -> IO (SEXP R.Any)


funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> SEXP c
funToSEXP w x = unsafePerformIO $ fmap castPtr . funPtrToSEXP . castFunPtr
                =<< w (hFunWrap x)

$(thWrapperLiterals 4 25)
