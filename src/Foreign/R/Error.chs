-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.R.Error
  ( RError(..)
  , failure
  , violation
  , impossible
  , unimplemented
  ) where

import Control.Exception
import Data.Typeable

data RError
      = RError String
      | Violation String String
      | Failure String String
      deriving ( Typeable )

instance Show RError where
  show (RError s)      = "R Runtime Error: " ++ s
  show (Failure f m)   = "User error:" ++ f ++ ":" ++ m
  show (Violation f m) = "Bug in " ++ f ++ ", please report: " ++ m

instance Exception RError

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
