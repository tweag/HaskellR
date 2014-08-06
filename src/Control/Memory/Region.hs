-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE TypeFamilies #-}
module Control.Memory.Region where

import GHC.Exts (Constraint)

data GlobalRegion

data Void

-- | Convenient shorthand.
type G = GlobalRegion

-- | Convenient shorthand.
type V = Void

-- | A partial order on regions. In fact regions form a lattice, with
-- 'GlobalRegion' being the supremum and 'Void' the infimum.
type family   a <= b :: Constraint
type instance a <= a = ()
type instance a <= G = ()
type instance V <= b = ()
