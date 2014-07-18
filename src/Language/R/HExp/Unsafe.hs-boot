#if __GLASGOW_HASKELL__ > 706
{-# LANGUAGE RoleAnnotations #-}
#endif
module Language.R.HExp.Unsafe where

import Foreign.R.Type (SEXPTYPE)

#if __GLASGOW_HASKELL__ > 706
type role HExp nominal
#endif
data HExp :: SEXPTYPE -> *
