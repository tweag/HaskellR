-- |
-- Copyright: (C) 2013 Amgen, Inc.
module H.Prelude where

import Data.Some
import qualified Foreign.R as R

liftR :: (R.SEXP a -> b) -> Some R.SEXP -> b
liftR f (Some x) = f (R.SEXP . R.unSEXP $ x)
