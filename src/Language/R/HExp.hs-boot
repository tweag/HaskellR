{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
module Language.R.HExp where

import Foreign.R.Type (SEXPTYPE)

#if __GLASGOW_HASKELL__ >= 708
type role HExp phantom nominal
#endif
data HExp :: * -> SEXPTYPE -> *
