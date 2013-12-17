-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Predicates seen in type signatures in H.

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverlappingInstances #-}

module H.Constraints where

infix 1 :∈
infixr 2 :+:

-- | Combine classes. This datatype is used purely as a type index, has no
-- computational significance and hence has no runtime representation.
data a :+: b

-- | Type level lists not nil terminated. This is a syntactic convenience, even
-- if it means we need slightly more instances than would otherwise be needed
-- below. In any case, making empty lists representable would duplicate the lub
-- of all classes (e.g. 'Foreign.R.Any' in R).
class a :∈ b
instance a :∈ a
instance a :∈ (a :+: b)
instance (a :∈ c) => a :∈ (b :+: c)

-- | Class alias used for c2hs @fun@ hooks, since it currently does not like
-- Unicode operators.
type In a b = a :∈ b

-- | Heterogeneous equality.
class HEq (t :: k -> *) where
  (===) :: t a -> t b -> Bool
