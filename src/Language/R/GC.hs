-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides helper functions and structures for safe memory
-- management when communicating with R.
--
--
module Language.R.GC
  ( preserve
  , preserveSome
  , withProtected
  ) where

import Control.Memory.Region
import H.Internal.Prelude
import qualified Foreign.R as R
import System.Mem.Weak (addFinalizer)

import Control.Monad.Catch ( MonadCatch, MonadMask, bracket )
import Control.Monad.Trans ( MonadIO(..) )

-- | Store an object in Preserved region.
preserve :: MonadR m => R.SEXP s a -> m (R.SEXP G a)
preserve s = io $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject (R.unsafeRelease s'))
    return s'
  where
    s' = R.unsafeRelease s

-- | Store object in Preserved region
preserveSome :: MonadR m => R.SomeSEXP s -> m (R.SomeSEXP G)
preserveSome (SomeSEXP s) = io  $ do
    R.preserveObject s'
    post <- getPostToCurrentRThread
    s' `addFinalizer` (post $ R.releaseObject s')
    return $ SomeSEXP s' 
  where
    s' = R.unsafeRelease s

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: (MonadIO m, MonadCatch m, MonadMask m)
              => m (R.SEXP V a)      -- Action to acquire resource
              -> (R.SEXP s a -> m b) -- Action
              -> m b
withProtected create f =
    bracket
      (do { x <- create; _ <- liftIO $ R.protect x; return x })
      (const $ liftIO $ R.unprotect 1)
      (f . R.unsafeRelease)
