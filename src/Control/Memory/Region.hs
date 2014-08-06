-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE TypeFamilies #-}
module Control.Memory.Region where

import GHC.Exts (Constraint)

data GlobalRegion
data Void

type G = GlobalRegion
type V = Void

type family   IsAncestorOf a b :: Constraint
type instance IsAncestorOf G x = ()
type instance IsAncestorOf x V = ()
