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
type family   IsAncestorOf a b :: Bool
type instance IsAncestorOf G x = True
type instance IsAncestorOf x V = True
