-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- R-specific predicates for encoding form constraints in type signatures. There
-- are no actual bindings in this module.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Foreign.R.Constraints where

import GHC.Exts (Constraint)
import {-# SOURCE #-} Foreign.R.Type (SEXPTYPE(..))

infix 1 :∈

-- | The predicate @a :∈ as@ states that @a@ is a member type of the set @as@.
type family (a :: SEXPTYPE) :∈ (as :: [SEXPTYPE]) :: Constraint where
  'Any :∈ as = ()
  a :∈ (a ': as) = ()
  a :∈ (b ': as) = a :∈ as

-- | Class alias used for c2hs @fun@ hooks, since it currently does not like
-- Unicode operators.
type In a b = a :∈ b
