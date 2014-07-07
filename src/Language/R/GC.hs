-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides helper functions and structures for safe memory
-- management when communicating with R.
--
-- 'RVal' is a wrapper around 'SEXP' preventing the 'SEXP' from being garbage
-- collected by R until the Haskell garbage collector signals that it is safe to
-- do so.

{-# LANGUAGE CPP #-}

module Language.R.GC
  ( -- * RVal
    RVal
  , newRVal
  , touchRVal
  , withRVal
  , unprotectRVal
    -- * SomeRVal
  , SomeRVal
  , newSomeRVal
  , touchSomeRVal
  , withSomeRVal
  , unprotectSomeRVal
    -- * Helpers
  , withProtected
  ) where

import H.Internal.Prelude
import Control.Applicative
import Foreign ( ForeignPtr, touchForeignPtr, finalizeForeignPtr, castPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Concurrent ( newForeignPtr )
import qualified Foreign.R.Internal as R

#if MIN_VERSION_exceptions(0,6,0)
import Control.Monad.Catch ( MonadCatch, MonadMask, bracket )
#else
import Control.Monad.Catch ( MonadCatch, bracket )
#endif
import Control.Monad.Trans ( MonadIO(..) )

-- | An 'RVal' is a reference to a /protected/ R object that is maintained by
-- R storage memory. If GHC's GC determines that the object has become
-- unreachable from Haskell, then object is unprotected.
newtype RVal (a :: R.SEXPTYPE) = RVal (ForeignPtr R.SEXPREC)

-- | Create R value and automatically protect it.
newRVal :: MonadR m => R.SEXP a -> m (RVal a)
newRVal s = io $ do
    R.preserveObject s
    post <- getPostToCurrentRThread
    fp <- newForeignPtr (R.unsexp s) (post $ R.releaseObject (castPtr $ R.unsexp s))
    return (RVal fp)

-- | Keep SEXP value from the garbage collection.
touchRVal :: MonadR m => RVal a -> m ()
touchRVal (RVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object.
withRVal :: MonadR m => RVal a -> (R.SEXP a -> m b) -> m b
withRVal (RVal s) f = do
        let s' = unsafeForeignPtrToPtr s
        x <- f (R.sexp s')
        io $ touchForeignPtr s
        return x

-- | Unprotect 'SEXP' in R memory. This doesn't mean that value will be
-- immideately deallocated, just that it may be deallocated on the next GC.
unprotectRVal :: MonadR m => RVal a -> m ()
unprotectRVal (RVal s) = io (finalizeForeignPtr s)

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
#if MIN_VERSION_exceptions(0,6,0)
withProtected :: (MonadIO m, MonadCatch m, MonadMask m)
#else
withProtected :: (MonadIO m, MonadCatch m)
#endif
              => m (R.SEXP a)      -- Action to acquire resource
              -> (R.SEXP a -> m b) -- Action
              -> m b
withProtected acquire =
    bracket
      (do { x <- acquire; _ <- liftIO $ R.protect x; return x })
      (const $ liftIO $ R.unprotect 1)

-- | Non type indexed version of 'RVal'.
newtype SomeRVal = SomeRVal (ForeignPtr R.SEXPREC)

-- | Create R value and automatically protect it.
newSomeRVal :: MonadR m => SomeSEXP -> m SomeRVal
newSomeRVal (SomeSEXP s) = io $ do
    R.preserveObject s
    post <- getPostToCurrentRThread
    SomeRVal <$> newForeignPtr (R.unsexp s) (post $ R.releaseObject (castPtr $ R.unsexp s))

-- | Keep 'SEXP' value from the garbage collection.
touchSomeRVal :: MonadR m => SomeRVal -> m ()
touchSomeRVal (SomeRVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object.
withSomeRVal :: MonadR m => SomeRVal -> (SomeSEXP -> m b) -> m b
withSomeRVal (SomeRVal s) f = do
        let s' = unsafeForeignPtrToPtr s
        x <- f (SomeSEXP (R.sexp s'))
        io $ touchForeignPtr s
        return x

-- | Unprotect 'SEXP' in R memory. This doesn't mean that value will be
-- immediately deallocated, just that it may be deallocated on the next GC.
unprotectSomeRVal :: MonadR m => SomeRVal -> m ()
unprotectSomeRVal (SomeRVal s) = io (finalizeForeignPtr s)
