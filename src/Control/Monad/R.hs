-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- = The R monad
--
-- The R Monad provides a safe way to work with R values. The R monad manages
-- protection of values under the hood, so that the R garbage collector does not
-- deallocate them prematurely.
--
-- The idea is to track allocation of R values within a /region/. Within the
-- scope of the region, values are protected and hence guaranteed not to be
-- freed. Once control flow exits the region, the values allocated within that
-- region are no longer protected, and hence susceptible to being freed by R's
-- garbage collector.
--
-- The current region is identified by a dummy index type variable @s@ to the
-- R monad. That is, in the type @R s a@, @s@ marks the current region. Use
-- 'runRegion' to open a new region, e.g.
--
-- > runRegion $ do
-- >   x <- fromSEXP <$> protect foo
-- >   y <- fromSEXP <$> protect bar
-- >   foo (x,y)
--
-- Here @x@ and @y@ will be protected during the computation of @foo@ and will
-- be unprotected just before a return.
--
-- __Implementation notes:__
--
--   * The current solution does not support nested regions, so passing a value
--   to the child region from a parent is not possible even it's safe.
--
-- = Liberating values
--
-- All R values are bound to a region. However, it is sometimes necessary to
-- communicate a value to remote threads, which will not in general be part of
-- the same region. This is possible once a value is *liberated* from its native
-- region.

{-# LANGUAGE Rank2Types #-}

module Control.Monad.R
  ( -- * The R monad
    R
  , io
  , MonadR
  , Unsafe.withR
    -- * Regions
    -- ** Execution
  , runR
  , runRegion
  , SubRegion(..)
  , newRegion
    -- * Protection
  , Protect(..)
  , Unprotect(..)
  , Unsafe.UnsafeValue
  , Unsafe.unsafeUseValue
    -- * Operations lifting
  , Foreign.R.liftProtect
  ) where

import Control.Monad.R.Class
import Foreign.R
import           Control.Monad.R.Unsafe (R(..))
import qualified Control.Monad.R.Unsafe as Unsafe

import Control.Monad.Reader
import Data.IORef

-- This guarantee safeness of values usafe inside a region.
--
-- 'Unprotect' constraint allow to run unprotect procedure on the values that
-- leaving a channel.
runRegion :: (Unprotect a) => (forall s. R s a) -> R s' (UnprotectElt a)
runRegion = R . ReaderT . const . runR

-- | Run R region inside an IO monad.
--
-- This method will not allow to to return a values that have an @s@ variable
runR :: (Unprotect a) => (forall s . R s a) -> IO (UnprotectElt a)
runR f = Unsafe.runR (unprotect =<< f)

-- | A witness that the region 'r' is older than
-- (or, is the parent of, the subtype of) the region labeled 's'
newtype SubRegion r s = SubRegion (forall v . R r v -> R s v)

-- | A function that allow to share variables from the partent region in the current
-- one. This apporach slightly differs from the one that was originaly described in
-- papar by Oleg Kiselov.
--
-- The main reason it that R protection stack doesn't allow to register values inside
-- a parent region, so every value even tagged as registered in a parent region will
-- be unprotected when it leaves the scope of the function. However this is safe as
-- 'Unprotect' constraint cleares information about region where value was protected.
--
-- Usage:
--
-- > newRegion $ (SubRegion witness) -> do
-- >   z <-witness $ getAttribute (x {- value from the parent region -})
-- >   use z
--
newRegion :: (Unprotect v) => (forall s . SubRegion r s -> R s v) -> R r (UnprotectElt v)
newRegion body = R $ do
   -- Create a counter for a new region
   t <- ReaderT $ const $ newIORef 0
   -- Create a 'witness' function that changes the label of the region. this function
   -- unsafely runs a function from the parent region with _current_ counter. The
   -- result will be tagged with the current region
   let witness (R m) = R $ ReaderT $ \_ -> runReaderT m t
   ReaderT $ const $ Unsafe.runRWith (unprotect =<< body (SubRegion witness)) t
