{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: 2018 (C) Tweag I/O Limited.
--
-- inline-c context.
module Foreign.R.Context
  ( rCtx
  , SEXPREC
  , SEXP0(..)
  , Logical(..)
  ) where

import Data.Complex
import qualified Data.Map as Map
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Language.C.Types (TypeSpecifier(TypeName))
import Language.C.Inline.Context (Context(..))
import Internal.Error

#include <Rinternals.h>

data SEXPREC

newtype {-# CTYPE "SEXP" #-} SEXP0 = SEXP0 { unSEXP0 :: Ptr SEXPREC }
  deriving ( Eq
           , Ord
           , Storable
           )

instance Show SEXP0 where
  show (SEXP0 ptr) = show ptr

-- | R uses three-valued logic.
data {-# CTYPE "Logical" #-} Logical = FALSE
             | TRUE
             | NA
-- XXX no Enum instance because NA = INT_MIN, not representable as an Int on
-- 32-bit systems.
               deriving (Eq, Ord, Show)

instance Storable Logical where
  sizeOf _       = sizeOf (undefined :: CInt)
  alignment _    = alignment (undefined :: CInt)
  poke ptr FALSE = poke (castPtr ptr) (0 :: CInt)
  poke ptr TRUE  = poke (castPtr ptr) (1 :: CInt)
  -- Currently NA_LOGICAL = INT_MIN.
  poke ptr NA    = poke (castPtr ptr) (#{const INT_MIN} :: CInt)
  peek ptr = do
      x <- peek (castPtr ptr)
      case x :: CInt of
          0 -> return FALSE
          1 -> return TRUE
          #{const INT_MIN} -> return NA
          _ -> failure "Storable Logical peek" "Not a Logical."

rCtx :: Context
rCtx = mempty { ctxTypesTable = Map.fromList tytabs }
  where
    tytabs =
      [ (TypeName "SEXP", [t| SEXP0 |])
      , (TypeName "Rcomplex", [t| Complex Double |])
      ]
