-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Unityped representation of the R values in the
-- Haskell runtime,

module H.Value
  where


-- | Description of the RValues.
data RValue = RNil    -- ^ Nil value
            deriving (Eq, Show)
