-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.R.Class
  ( MonadR(..)
  , acquireSome
  ) where

import Control.Memory.Region
import Foreign.R

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Trans (MonadIO(..))

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'R' monad. For convenience, in a GHCi session, we normally use the
-- 'IO' monad directly (by means of a 'MonadR' instance for 'IO', imported only
-- in GHCi).
class (Applicative m, MonadIO m, MonadCatch m, MonadMask m) => MonadR m where
  type Region m :: *
  type Region m = G
  -- | Lift an 'IO' action.
  io :: IO a -> m a
  io = liftIO
  -- | Accuire
  acquire :: SEXP V a -> m (SEXP (Region m) a)
  default acquire :: (MonadIO m, Region m ~ G) => SEXP V a -> m (SEXP G a)
  acquire = liftIO . protect

-- | Helper that allow to protect @SomeSEXP s@ inside a region.
acquireSome :: (MonadR m) => SomeSEXP V -> m (SomeSEXP (Region m))
acquireSome (SomeSEXP s) = SomeSEXP <$> acquire s
