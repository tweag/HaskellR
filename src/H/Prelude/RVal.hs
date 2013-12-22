-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- RVal is a value supposed to hold SEXP that are in haskell
-- memory from garbage collection on in R.
{-# LANGUAGE GADTs #-}
module H.Prelude.RVal
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
  ) where

import H.Internal.Prelude
import Control.Applicative
import Foreign hiding ( newForeignPtr, unsafeForeignPtrToPtr )
import Foreign.Concurrent
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import qualified Foreign.R as R

{-
- We are switching to Foreign.Concurrent instead on Foreign.ForeignPtr
foreign import ccall "wrapper" mkUnprotect ::
  (Ptr a -> IO ()) -> IO (FinalizerPtr a)

unprotectSEXP :: SEXP
unprotectSEXP = unsafePerformIO $ mkUnprotect (R.unprotectPtr . R.unsexp)
{-# NOINLINE unprotectSEXP #-}
-}
-- | The type "RVal" represent reference to R object that is
-- maintained by R storage memory. RValue automatically 'Foreign.R.protect'
-- object and 'Foreign.R.unprotectPtr' it when it becomes unavailable.
data RVal :: SEXPTYPE -> * where
        RVal :: ForeignPtr R.SEXPREC -> RVal a

-- | Create R value and automatically protect it
newRVal :: MonadR m => SEXP a -> m (RVal a)
newRVal s = io $ do
    _ <- R.protect s
    fp <- newForeignPtr (R.unsexp s) (R.unprotectPtr s)
    return (RVal fp)

-- | Keep SEXP value from the garbage collection
touchRVal :: MonadR m => RVal a -> m ()
touchRVal (RVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object
withRVal :: MonadR m => RVal a -> (SEXP a -> m b) -> m b
withRVal (RVal s) f = do
        let s' = unsafeForeignPtrToPtr s
        x <- f (R.sexp s')
        io $ touchForeignPtr s
        return x

-- | Unprotect "SEXP" in R memory. This doesn't mean that value will be
-- immideately deallocated, just that it may be deallocated on the next GC.
unprotectRVal :: MonadR m => RVal a -> m ()
unprotectRVal (RVal s) = io (finalizeForeignPtr s)

-- | The type "RVal" represent reference to R object that is
-- maintained by R storage memory. RValue automatically 'Foreign.R.protect'
-- object and 'Foreign.R.unprotectPtr' it when it becomes unavailable.
data SomeRVal = SomeRVal (ForeignPtr R.SEXPREC)

-- | Create R value and automatically protect it
newSomeRVal :: MonadR m => SomeSEXP -> m SomeRVal
newSomeRVal (SomeSEXP s) = io $ do
    _ <- R.protect s
    SomeRVal <$> newForeignPtr (R.unsexp s) (R.unprotectPtr s)

-- | Keep SEXP value from the garbage collection
touchSomeRVal :: MonadR m => SomeRVal -> m ()
touchSomeRVal (SomeRVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object
withSomeRVal :: MonadR m => SomeRVal -> (SomeSEXP -> m b) -> m b
withSomeRVal (SomeRVal s) f = do 
        let s' = unsafeForeignPtrToPtr s
        x <- f (SomeSEXP (R.sexp s'))
        io $ touchForeignPtr s
        return x

-- | Unprotect "SEXP" in R memory. This doesn't mean that value will be
-- immideately deallocated, just that it may be deallocated on the next GC.
unprotectSomeRVal :: MonadR m => SomeRVal -> m ()
unprotectSomeRVal (SomeRVal s) = io (finalizeForeignPtr s)
