-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP #-}

module Control.Monad.R.Class
  ( MonadR(..)
  ) where

import Control.Applicative (Applicative)
#if MIN_VERSION_exceptions(0,6,0)
import Control.Monad.Catch (MonadCatch, MonadMask)
#else
import Control.Monad.Catch (MonadCatch)
#endif
import Control.Monad.Trans (MonadIO(..))

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'R' monad. For convenience, in a GHCi session, we normally use the
-- 'IO' monad directly (by means of a 'MonadR' instance for 'IO', imported only
-- in GHCi).
#if MIN_VERSION_exceptions(0,6,0)
class (Applicative m, MonadIO m, MonadCatch m, MonadMask m) => MonadR m where
#else
class (Applicative m, MonadIO m, MonadCatch m) => MonadR m where
#endif
  -- | Lift an 'IO' action.
  io :: IO a -> m a
  io = liftIO
