-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# Language ConstraintKinds #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language ViewPatterns #-}

module Language.R.Literal.Unsafe
  ( Literal(..)
  , mkSEXPVector
  , mkSEXPVectorIO
  , mkProtectedSEXPVector
  , mkProtectedSEXPVectorIO
  ) where

import           H.Internal.Error
import           Language.R.HExp.Unsafe
import qualified Data.Vector.SEXP as SVector
import           Foreign.R.Internal (SEXP, SomeSEXP(..), withProtected)
import qualified Foreign.R.Internal as R
import           Foreign.R.Type ( IsVector, SSEXPTYPE )

import Data.Singletons ( SingI, fromSing, sing )

import Control.Monad ( void, zipWithM_ )
import Data.Int (Int32)
import Data.Complex (Complex)
import Foreign ( castPtr )
import Foreign.C.String ( newCString )
import Foreign.Storable ( Storable, pokeElemOff )
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'SEXP'.
class Literal a b | a -> b where
    unsafeMkSEXP :: a -> IO (SEXP b)
    fromSEXP :: SEXP c -> a


{-# NOINLINE mkSEXPVector #-}
mkSEXPVector :: (Storable (SVector.ElemRep a), IsVector a)
             => SSEXPTYPE a
             -> [SVector.ElemRep a]
             -> SEXP a
mkSEXPVector ty xs = unsafePerformIO $ mkSEXPVectorIO ty xs

mkSEXPVectorIO :: (Storable (SVector.ElemRep a), IsVector a)
               => SSEXPTYPE a
               -> [SVector.ElemRep a]
               -> IO (SEXP a)
mkSEXPVectorIO ty xs =
    withProtected (R.allocVector ty $ length xs) $ \vec -> do
      let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
      zipWithM_ (pokeElemOff ptr) [0..] xs
      return vec

{-# NOINLINE mkProtectedSEXPVector #-}
mkProtectedSEXPVector :: IsVector b
                      => SSEXPTYPE b
                      -> [SEXP a]
                      -> SEXP b
mkProtectedSEXPVector ty xs = unsafePerformIO $ mkProtectedSEXPVectorIO ty xs

mkProtectedSEXPVectorIO :: IsVector b
                        => SSEXPTYPE b
                        -> [SEXP a]
                        -> IO (SEXP b)
mkProtectedSEXPVectorIO ty xs = do
    mapM_ (void . R.protect) xs
    z <- withProtected (R.allocVector ty $ length xs) $ \vec -> do
           let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
           zipWithM_ (pokeElemOff ptr) [0..] xs
           return vec
    R.unprotect (length xs)
    return z

instance Literal [R.Logical] 'R.Logical where
    unsafeMkSEXP = mkSEXPVectorIO sing
    fromSEXP (hexp -> Logical v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal [Int32] R.Int where
    unsafeMkSEXP = mkSEXPVectorIO sing
    fromSEXP (hexp -> Int v) = SVector.toList v
    fromSEXP (hexp -> Real v) = map round (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal [Double] 'R.Real where
    unsafeMkSEXP = mkSEXPVectorIO sing
    fromSEXP (hexp -> Real v) = SVector.toList v
    fromSEXP (hexp -> Int v) = map fromIntegral (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal [Complex Double] R.Complex where
    unsafeMkSEXP = mkSEXPVectorIO sing
    fromSEXP (hexp -> Complex v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

-- | Named after eponymous "GHC.Exts" function.
the :: IsVector a => Literal [SVector.ElemRep a] a => SEXP a -> SVector.ElemRep a
the (fromSEXP -> xs)
  | length xs == 1 = head xs
  | otherwise = failure "the" "Not a singleton vector."

instance Literal R.Logical 'R.Logical where
    unsafeMkSEXP x = unsafeMkSEXP [x]
    fromSEXP x@(hexp -> Logical{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal Int32 R.Int where
    unsafeMkSEXP x = unsafeMkSEXP [x]
    fromSEXP x@(hexp -> Int{}) = the x
    fromSEXP x@(hexp -> Real{}) = round (the x)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal Double R.Real where
    unsafeMkSEXP x = unsafeMkSEXP [x]
    fromSEXP x@(hexp -> Real{}) = the x
    fromSEXP x@(hexp -> Int{})  = fromIntegral (the x)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal (Complex Double) R.Complex where
    unsafeMkSEXP x = unsafeMkSEXP [x]
    fromSEXP x@(hexp -> Complex{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

instance SingI a => Literal (SEXP a) a where
    unsafeMkSEXP  = return
    fromSEXP = R.cast (fromSing (sing :: SSEXPTYPE a)) . SomeSEXP

instance Literal SomeSEXP R.Any where
    -- The ANYSXP type in R plays the same role as SomeSEXP in H. It is a dummy
    -- type tag, that is never seen in any object. It serves only as a stand-in
    -- when the real type is not known.
    unsafeMkSEXP (SomeSEXP s) = return $ R.unsafeCoerce s
    fromSEXP = SomeSEXP

instance Literal String R.String where
    unsafeMkSEXP x = R.mkString =<< newCString x
    fromSEXP  = unimplemented "Literal String fromSEXP"

