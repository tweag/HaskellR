-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Wrappers around 'error' that classify problems into whether these are bugs
-- internal to H, or whether they are due to a mistake by the user.
--
-- This module should only be imported by H modules and not reexported.

module H.Internal.Error
  ( failure
  , violation
  , impossible
  , unimplemented
  ) where

-- | User error.
failure :: String                                 -- ^ Function name
        -> String                                 -- ^ Error message
        -> a
failure f msg = error $ f ++ ": " ++ msg

-- | An internal invariant has been violated. That's a bug.
violation :: String                               -- ^ Function name
          -> String                               -- ^ Error message
          -> a
violation f msg = error $ "Bug in " ++ f ++ ", please report: " ++ msg

-- | A violation that should have been made impossible by the type system was
-- not.
impossible :: String                               -- ^ Function name
           -> a
impossible f = violation f "The impossible happened."

-- | Feature not yet implemented.
unimplemented :: String
              -> a
unimplemented f = failure f "Unimplemented."
