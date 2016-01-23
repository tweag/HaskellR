-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Phantom type indices for segregating values into "regions" of memory, which
-- are markers that serve as static conservative approximations of the liveness
-- of an object. That is, regions have scopes, and objects within a region are
-- guaranteed to remain live within the scope of that region.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Memory.Region where

import GHC.Exts (Constraint, RealWorld)

-- | The global region is a special region whose scope extends all the way to
-- the end of the program. As such, any object allocated within this region
-- lives "forever". In this sense, it is the top-level region, whose scope
-- includes all other regions.
type GlobalRegion = RealWorld

-- | Void is not a region. It is a placeholder marking the absence of region.
-- Useful to tag objects that belong to no region at all.
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
