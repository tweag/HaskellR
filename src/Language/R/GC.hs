-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- RVal is a value supposed to hold SEXP that are in haskell
-- memory from garbage collection on in R.
{-# LANGUAGE GADTs #-}
module Language.R.GC
  ( RVal
  , newRVal
  , touchRVal
  , withRVal
  , unprotectRVal
  , withProtected
  ) where

import Foreign hiding ( newForeignPtr, unsafeForeignPtrToPtr )
import Foreign.Concurrent
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import qualified Foreign.R as R
import Control.Monad.R.Class

import Control.Exception ( bracket )

-- | The type "RVal" represent reference to R object that is
-- maintained by R storage memory. RValue automatically 'Foreign.R.protect'
-- object and 'Foreign.R.unprotectPtr' it when it becomes unavailable.
data RVal :: R.SEXPTYPE -> * where
  RVal :: ForeignPtr R.SEXPREC -> RVal a

-- | Create R value and automatically protect it
newRVal :: MonadR m => R.SEXP a -> m (RVal a)
newRVal s = io $ do
    _ <- R.protect s
    fp <- newForeignPtr (R.unsexp s) (R.unprotectPtr s)
    return (RVal fp)

-- | Keep SEXP value from the garbage collection
touchRVal :: MonadR m => RVal a -> m ()
touchRVal (RVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object
withRVal :: MonadR m => RVal a -> (R.SEXP a -> m b) -> m b
withRVal (RVal s) f = do
        let s' = unsafeForeignPtrToPtr s
        x <- f (R.sexp s')
        io $ touchForeignPtr s
        return x

-- | Unprotect "SEXP" in R memory. This doesn't mean that value will be
-- immideately deallocated, just that it may be deallocated on the next GC.
unprotectRVal :: MonadR m => RVal a -> m ()
unprotectRVal (RVal s) = io (finalizeForeignPtr s)

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: IO (R.SEXP a)      -- Action to acquire resource
              -> (R.SEXP a -> IO b) -- Action
              -> IO b
withProtected accure =
   bracket (accure >>= \x -> R.protect x >> return x)
           (const (R.unprotect 1))
