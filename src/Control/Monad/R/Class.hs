-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

module Control.Monad.R.Class
  ( MonadR(..)
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Trans (MonadIO(..))

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'R' monad. For convenience, in a GHCi session, we normally use the
-- 'IO' monad directly (by means of a 'MonadR' instance for 'IO', imported only
-- in GHCi).
class (Applicative m, MonadIO m, MonadCatch m, MonadMask m) => MonadR m where
  -- | Lift an 'IO' action.
  io :: IO a -> m a
  io = liftIO
