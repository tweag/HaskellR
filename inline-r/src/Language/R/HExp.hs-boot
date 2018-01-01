{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
module Language.R.HExp where

import Foreign.R.Type (SEXPTYPE)

type role HExp phantom nominal
data HExp :: * -> SEXPTYPE -> *
