{-# LANGUAGE RoleAnnotations #-}
module Language.R.HExp where

import Foreign.R.Type (SEXPTYPE)

#if __GLASGOW_HASKELL__ > 706
type role HExp nominal
#endif
data HExp :: SEXPTYPE -> *
