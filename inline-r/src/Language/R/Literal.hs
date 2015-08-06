-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language ViewPatterns #-}

module Language.R.Literal
  ( Literal(..)
  , mkSEXP
  , mkSEXPVector
  , mkSEXPVectorIO
  , HFunWrap(..)
  , funToSEXP
  , mkProtectedSEXPVector
  , mkProtectedSEXPVectorIO
    -- * wrapper helpers
  ) where

import           Control.Memory.Region
import           H.Internal.Prelude
import           Language.R.HExp
import           Language.R.Internal.FunWrappers
import           Language.R.Internal.FunWrappers.TH
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.SEXP.Mutable as SMVector
import qualified Foreign.R as R
import           Foreign.R.Type ( IsVector, SSEXPTYPE )

import Data.Singletons ( SingI, sing )

import Control.Monad ( void, zipWithM_ )
import Data.Char (chr)
import Data.Int (Int32)
import Data.Complex (Complex)
import Foreign          ( FunPtr, castPtr )
import Foreign.C.String ( withCString )
import Foreign.Storable ( Storable, pokeElemOff )
import System.IO.Unsafe ( unsafePerformIO )

-- | Values that can be converted to 'SEXP'.
class Literal a b | a -> b where
    -- | Internal function for converting a literal to a 'SEXP' value. You
    -- probably want to be using 'mkSEXP' instead.
    mkSEXPIO :: a -> IO (SEXP V b)
    fromSEXP :: SEXP s c -> a

-- |  Create a SEXP value and protect it in current region
mkSEXP :: (Literal a b, MonadR m) => a -> m (SEXP (Region m) b)
mkSEXP x = acquire =<< io (mkSEXPIO x)

{-# NOINLINE mkSEXPVector #-}
mkSEXPVector :: (Storable (SVector.ElemRep s a), IsVector a)
             => SSEXPTYPE a
             -> [SVector.ElemRep s a]
             -> SEXP s a
mkSEXPVector ty xs = unsafePerformIO $ mkSEXPVectorIO ty xs

mkSEXPVectorIO :: (Storable (SVector.ElemRep s a), IsVector a)
               => SSEXPTYPE a
               -> [SVector.ElemRep s a]
               -> IO (SEXP s a)
mkSEXPVectorIO ty xs =
    R.withProtected (R.allocVector ty $ length xs) $ \vec -> do
      let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
      zipWithM_ (pokeElemOff ptr) [0..] xs
      return vec

{-# NOINLINE mkProtectedSEXPVector #-}
mkProtectedSEXPVector :: IsVector b
                      => SSEXPTYPE b
                      -> [SEXP s a]
                      -> SEXP s b
mkProtectedSEXPVector ty xs = unsafePerformIO $ mkProtectedSEXPVectorIO ty xs

mkProtectedSEXPVectorIO :: IsVector b
                        => SSEXPTYPE b
                        -> [SEXP s a]
                        -> IO (SEXP s b)
mkProtectedSEXPVectorIO ty xs = do
    mapM_ (void . R.protect) xs
    z <- R.withProtected (R.allocVector ty $ length xs) $ \vec -> do
           let ptr = castPtr $ R.unsafeSEXPToVectorPtr vec
           zipWithM_ (pokeElemOff ptr) [0..] xs
           return vec
    R.unprotect (length xs)
    return z

instance Literal [R.Logical] 'R.Logical where
    mkSEXPIO = mkSEXPVectorIO sing
    fromSEXP (hexp -> Logical v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal [Int32] 'R.Int where
    mkSEXPIO = mkSEXPVectorIO sing
    fromSEXP (hexp -> Int v) = SVector.toList v
    fromSEXP (hexp -> Real v) = map round (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal [Double] 'R.Real where
    mkSEXPIO = mkSEXPVectorIO sing
    fromSEXP (hexp -> Real v) = SVector.toList v
    fromSEXP (hexp -> Int v) = map fromIntegral (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal [Complex Double] 'R.Complex where
    mkSEXPIO = mkSEXPVectorIO sing
    fromSEXP (hexp -> Complex v) = SVector.toList v
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

instance Literal [String] 'R.String where
    mkSEXPIO xs = mkSEXPVectorIO sing =<< mapM (`withCString` R.mkCharCE R.CE_UTF8) xs
    fromSEXP (hexp -> String v) =
        map (\(hexp -> Char xs) -> map (chr . fromIntegral) $ SVector.toList xs) (SVector.toList v)
    fromSEXP _ =
        failure "fromSEXP" "String expected where some other expression appeared."

instance Literal String 'R.Char where
    mkSEXPIO x = withCString x (R.mkCharCE R.CE_UTF8)
    fromSEXP (hexp -> Char xs) = map (chr . fromIntegral) $ SVector.toList xs
    fromSEXP _ =
        failure "fromSEXP" "String expected where some other expression appeared."

instance SVector.VECTOR V ty a => Literal (SVector.Vector V ty a) ty where
    mkSEXPIO = SVector.toSEXP
    fromSEXP = unsafePerformIO . SVector.freeze . fromSEXP

instance SVector.VECTOR V ty a => Literal (SMVector.IOVector V ty a) ty where
    mkSEXPIO = return . SMVector.toSEXP
    fromSEXP =
        SMVector.fromSEXP .
        R.cast (sing :: SSEXPTYPE ty) .
        SomeSEXP .
        R.release

-- | Named after eponymous "GHC.Exts" function.
the :: IsVector a => Literal [SVector.ElemRep s a] a => SEXP s a -> SVector.ElemRep s a
the (fromSEXP -> xs)
  | length xs == 1 = head xs
  | otherwise = failure "the" "Not a singleton vector."

instance Literal R.Logical 'R.Logical where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP x@(hexp -> Logical{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Logical expected where some other expression appeared."

instance Literal Int32 'R.Int where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP x@(hexp -> Int{}) = the x
    fromSEXP x@(hexp -> Real{}) = round (the x)
    fromSEXP _ =
        failure "fromSEXP" "Int expected where some other expression appeared."

instance Literal Double 'R.Real where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP x@(hexp -> Real{}) = the x
    fromSEXP x@(hexp -> Int{})  = fromIntegral (the x)
    fromSEXP _ =
        failure "fromSEXP" "Numeric expected where some other expression appeared."

instance Literal (Complex Double) 'R.Complex where
    mkSEXPIO x = mkSEXPIO [x]
    fromSEXP x@(hexp -> Complex{}) = the x
    fromSEXP _ =
        failure "fromSEXP" "Complex expected where some other expression appeared."

instance SingI a => Literal (SEXP s a) a where
    mkSEXPIO = fmap R.unsafeRelease . return
    fromSEXP = R.cast (sing :: SSEXPTYPE a) . SomeSEXP . R.unsafeRelease

instance Literal (SomeSEXP s) 'R.Any where
    -- The ANYSXP type in R plays the same role as SomeSEXP in H. It is a dummy
    -- type tag, that is never seen in any object. It serves only as a stand-in
    -- when the real type is not known.
    mkSEXPIO (SomeSEXP s) = return . R.unsafeRelease $ R.unsafeCoerce s
    fromSEXP = SomeSEXP . R.unsafeRelease

instance Literal a b => Literal (R s a) 'R.ExtPtr where
    mkSEXPIO = funToSEXP wrap0
    fromSEXP = unimplemented "Literal (R s a) fromSEXP"

instance (Literal a a0, Literal b b0) => Literal (a -> R s b) 'R.ExtPtr where
    mkSEXPIO = funToSEXP wrap1
    fromSEXP = unimplemented "Literal (a -> R s b) fromSEXP"

instance (Literal a a0, Literal b b0, Literal c c0)
         => Literal (a -> b -> R s c) 'R.ExtPtr where
    mkSEXPIO   = funToSEXP wrap2
    fromSEXP = unimplemented "Literal (a -> b -> IO c) fromSEXP"

-- | A class for functions that can be converted to functions on SEXPs.
class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (R s a) (IO R.SEXP0) where
    hFunWrap a = fmap R.unsexp $ (mkSEXPIO $!) =<< unsafeRToIO a

instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP0 -> wb) where
    hFunWrap f a = hFunWrap $ f $! fromSEXP (R.sexp a :: SEXP s la)

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr a -> IO (SEXP s 'R.ExtPtr)

funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> IO (SEXP s 'R.ExtPtr)
funToSEXP w x = funPtrToSEXP =<< w (hFunWrap x)

$(thWrapperLiterals 3 25)
