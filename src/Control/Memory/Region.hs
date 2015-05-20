-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- = Memory regions
--
-- Phantom type indices for segregating values into "regions" of memory, which
-- are markers that serve as static conservative approximations of the liveness
-- of an object. That is, regions have scopes, and objects within a region are
-- guaranteed to remain live within the scope of that region.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Memory.Region where

import GHC.Exts (Constraint)
import Control.Monad.R.Class

-- | The global region is a special region whose scope extends all the way to
-- the end of the program. As such, any object allocated within this region
-- lives "forever". In this sense, it is the top-level region, whose scope
-- includes all other regions.
data GlobalRegion

-- | Void is not a region. It is a placeholder marking the absence of region.
-- Useful to tag objects that belong to no region at all.
data Void

-- | Convenient shorthand.
type G = GlobalRegion

-- | Convenient shorthand.
type V = Void

-- | A partial order on regions. In fact regions form a lattice, with
-- 'GlobalRegion' being the supremum and 'Void' the infimum.
type family   a <= b :: Constraint where
      a <= a = ()
      a <= G = ()
      V <= b = ()

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s m a = R { unR :: ReaderT (IORef Int) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadMask, MonadThrow)

instance (Applicative m, MonadIO m, MonadMask m, MonadThrow m, MonadCatch m) => MonadR (R s m) where
  type Region (R s m) = s
  io m = R $ ReaderT $ \_ -> liftIO m
  acquire s = R $ ReaderT $ \cnt -> liftIO $ do
    x <- R.release <$> R.protect s
    modifyIORef' cnt succ
    return x
