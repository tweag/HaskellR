-- |
-- Copyright: 2013 (C) Amgen, Inc
--

module Language.R.Literal
   ( mkSEXP
   ) where

import           Foreign.R
import           Control.Monad.R (R)
import           Language.R.Literal.Unsafe (Literal(..))
import qualified Language.R.Literal.Unsafe as Unsafe

mkSEXP :: Literal a b => a -> R s (SEXP s b)
mkSEXP = liftProtect . Unsafe.unsafeMkSEXP
