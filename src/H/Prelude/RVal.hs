-- |
-- Copyright: (C) 2013 Amgen, Inc.
-- 
-- RVal is a value supposed to hold SEXP that are in haskell 
-- memory from garbage collection on in R.
{-# LANGUAGE GADTs #-}
module H.Prelude.RVal
  ( RVal
  , newRVal
  , touchRVal
  , withRVal
  , unprotectRVal
  ) where

import         H.Monad
import Foreign hiding ( newForeignPtr, unsafeForeignPtrToPtr )
import Foreign.Concurrent
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.R ( SEXP )
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
data RVal :: R.SEXPTYPE -> * where
        RVal :: ForeignPtr R.SEXPREC -> RVal a

-- | Create R value and automatically protect it
newRVal :: SEXP a -> R (RVal a)
newRVal s = io $ do
    _ <- R.protect s
    fp <- newForeignPtr (R.unsexp s) (R.unprotectPtr s)
    return (RVal fp)

-- | Keep SEXP value from the garbage collection
touchRVal :: RVal a -> R ()
touchRVal (RVal s) = io (touchForeignPtr s)

-- | This is a way to look inside RValue object
withRVal :: RVal a -> (R.SEXP a -> R b) -> R b
withRVal (RVal s) f = do
        let s' = unsafeForeignPtrToPtr s
        x <- f (R.sexp s')
        io $ touchForeignPtr s
        return x

-- | Unprotect "SEXP" in R memory. This doesn't mean that value will be
-- immideately deallocated, just that it may be deallocated on the next GC.
unprotectRVal :: RVal a -> R ()
unprotectRVal (RVal s) = io (finalizeForeignPtr s)
