-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.Literal
   ( mkSEXP
   , HFunWrap(..)
   , funToSEXP
   ) where

import           H.Internal.Error
import qualified Foreign.R.Internal as R
import           Foreign.R
import           Language.R.Literal.Unsafe (Literal(..))
import qualified Language.R.Literal.Unsafe as Unsafe
import           Language.R.Internal.FunWrappers
import           Language.R.Internal.FunWrappers.TH
import           Control.Monad.R.Unsafe (unsafeRToIO, UnsafeValue, unsafeUseValue)

import           Data.Singletons
import           Foreign          ( FunPtr )

mkSEXP :: Literal a b => a -> R s (SEXP s b)
mkSEXP = liftProtect . Unsafe.unsafeMkSEXP

instance SingI a => Literal (SEXP s a) a where
    unsafeMkSEXP  = return . unSEXP 
    fromSEXP = cast (fromSing (sing :: R.SSEXPTYPE a)) . SomeSEXP . SEXP

instance Literal (SomeSEXP s) Any where
    -- The ANYSXP type in R plays the same role as SomeSEXP in H. It is a dummy
    -- type tag, that is never seen in any object. It serves only as a stand-in
    -- when the real type is not known.
    unsafeMkSEXP (SomeSEXP s) = return $ unSEXP $ unsafeCoerce s
    fromSEXP = SomeSEXP . SEXP

instance Literal a b => Literal (UnsafeValue a) b where
    unsafeMkSEXP = flip unsafeUseValue unsafeMkSEXP
    fromSEXP = unimplemented "Literal (Unsafe a) fromSEXP"

instance Literal a b => Literal (R s a) R.ExtPtr where
    unsafeMkSEXP = funToSEXP wrap0
    fromSEXP = unimplemented "Literal (R a) fromSEXP"

instance (Literal a a0, Literal b b0) => Literal (a -> R s b) R.ExtPtr where
    unsafeMkSEXP = funToSEXP wrap1
    fromSEXP = unimplemented "Literal (a -> R b) fromSEXP"

instance (Literal a a0, Literal b b0, Literal c c0)
         => Literal (a -> b -> R s c) R.ExtPtr where
    unsafeMkSEXP   = funToSEXP wrap2
    fromSEXP = unimplemented "Literal (a -> b -> IO c) fromSEXP"

-- | A class for functions that can be converted to functions on SEXPs.
class HFunWrap a b | a -> b where
    hFunWrap :: a -> b

instance Literal a la => HFunWrap (R s a) (IO (R.SEXP la)) where
    hFunWrap a = (unsafeMkSEXP $!) =<< unsafeRToIO a

instance (Literal a la, HFunWrap b wb)
         => HFunWrap (a -> b) (R.SEXP la -> wb) where
    hFunWrap f a = hFunWrap $ f $! fromSEXP a

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr a -> IO (R.SEXP R.ExtPtr)

funToSEXP :: HFunWrap a b => (b -> IO (FunPtr b)) -> a -> IO (R.SEXP R.ExtPtr)
funToSEXP w x = funPtrToSEXP =<< w (hFunWrap x)

$(thWrapperLiterals 3 25)
