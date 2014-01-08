-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Wrappers around 'error' that classify problems into whether these are bugs
-- internal to H, or whether they are due to a mistake by the user.
--
-- This module should only be imported by H modules and not reexported.
{-# LANGUAGE DeriveDataTypeable #-}
module H.Internal.Error
  ( failure
  , violation
  , impossible
  , unimplemented
  ) where

import Control.Exception
import Data.Typeable

data Violation = Violation String String deriving ( Typeable )
data Failure = Failure String String deriving ( Typeable )

instance Show Failure where
  show (Failure f m)   = f ++ ":" ++ m

instance Show Violation where
  show (Violation f m) = "Bug in " ++ f ++ ", please report: " ++ m

instance Exception Violation
instance Exception Failure

-- | User error.
failure :: String                                 -- ^ Function name
        -> String                                 -- ^ Error message
        -> a
failure f msg = throw $ Failure f msg

-- | An internal invariant has been violated. That's a bug.
violation :: String                               -- ^ Function name
          -> String                               -- ^ Error message
          -> a
violation f msg = throw $ Violation f msg

-- | A violation that should have been made impossible by the type system was
-- not.
impossible :: String                               -- ^ Function name
           -> a
impossible f = violation f "The impossible happened."

-- | Feature not yet implemented.
unimplemented :: String
              -> a
unimplemented f = failure f "Unimplemented."
