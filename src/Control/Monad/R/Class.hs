-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.R.Class
  ( MonadR(..)
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Catch (MonadCatch, MonadMask)

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'R' monad. For convenience, in a GHCi session, we normally use the
-- 'IO' monad directly (by means of a 'MonadR' instance for 'IO', imported only
-- in GHCi).
class (Applicative m, MonadCatch m, MonadMask m) => MonadR m where

  type RRegion m :: *

  -- | Lift an 'IO' action.
  io :: IO a -> m a

  -- | Increment a counter for protected objects
  increment :: m ()
