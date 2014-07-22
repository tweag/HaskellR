-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE TypeFamilies #-}
module Control.Memory.Region where


data GlobalRegion
data Void

type G = GlobalRegion
type V = Void


-- | Relaction describes if Regions @a@ is ancestor
-- of region @b@. In terms of regions this means that
-- Any value from @a@ can safely be used in @b@.
type family   IsAncestor a b :: Bool
type instance IsAncestor G x = True
type instance IsAncestor x V = True
