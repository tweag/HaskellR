-- |
-- Copyright: 2013 (C) Amgen, Inc
--

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.Literal
   ( mkSEXP
   , module Unsafe 
   ) where

-- import           Foreign.R.Type
import           Foreign.R
import           Language.R.Literal.Unsafe (Literal(..))
import qualified Language.R.Literal.Unsafe as Unsafe

import           Data.Singletons

mkSEXP :: Literal a b => a -> R s (SEXP s b)
mkSEXP = liftProtect . Unsafe.unsafeMkSEXP

instance SingI a => Literal (SEXP s a) a where
    unsafeMkSEXP  = return . unSEXP 
    fromSEXP = error "fromSEXP for SEXP s a is not implemented yet"

instance Literal (SomeSEXP s) Any where
    -- The ANYSXP type in R plays the same role as SomeSEXP in H. It is a dummy
    -- type tag, that is never seen in any object. It serves only as a stand-in
    -- when the real type is not known.
    unsafeMkSEXP (SomeSEXP s) = return $ unSEXP $ unsafeCoerce s
    fromSEXP = error "fromSEXP for SomeSEXP s is not implemented yet"
