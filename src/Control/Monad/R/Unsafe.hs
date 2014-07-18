-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module contains unsafe primitives that can be used 
-- during the work with 'Control.Monad.R'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.R.Unsafe
   where

import           Foreign.R.Runner
import           Foreign.R.Internal as R
import           Control.Monad.R.Class

import 		 Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

import Control.Applicative
import Control.Exception ( bracket, bracket_, mask_ )
import Control.Monad.Reader
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s a = R { unR :: ReaderT (IORef Int) IO a }
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative, MonadReader (IORef Int))

instance MonadR (R s) where
  io m = unsafeIOToR $ unsafeRunInRThread m


-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance.
--
-- WARNING. due to the bug in R it's not possible to reinitialize R code, this
-- means that when this function will exit, then it will not be possible to
-- run it again or perform any R computaion.
--
-- WARNING. Return value is not forced, this means that it's may leak some
-- computations.
--
-- 'SEXP' from a different regions can not be shared.
runR :: Config -> (forall s. R s a) -> IO a
runR config r = bracket_ (initialize config) finalize (internalRunRegion r)

-- | A method that allowes to return from a region, can be used in callbacks SEXP.
protectRegion :: (forall s . R s (R.SEXP a)) -> R s' (R.SEXP a)
protectRegion = R . ReaderT . const . internalRunRegion

-- | Run a region without calling forcing return value
--
-- This action is unsafe because it does not guarantee that R runtime is initialized.
internalRunRegion :: (forall s.R s a) -> IO a
internalRunRegion f =
   bracket (newIORef 0)
           (readIORef >=> R.unprotect)
	   (runReaderT (unR f))

-- | Run an R action in the global R instance from the IO monad.
--
-- This method doesn't guarantee that R runtime is initialized.
--
-- This call doesn't affect R state so it can't protect or unprotect variables
-- use with the great caution.
unsafeRToIO :: R s a -> IO a
unsafeRToIO (R m) = runReaderT m =<< newIORef 0

-- | Lift IO action into R monad
unsafeIOToR :: IO a -> R s a
unsafeIOToR = R . ReaderT . const

-- | 'unsafePerformIO' analogue for R monad.
unsafePerformR :: R s a -> a
unsafePerformR r = unsafePerformIO $ unsafeRToIO r
{-# NOINLINE unsafePerformR #-}

-- | Protect SEXP inside a current region
protect :: R.SEXP a -> R s (R.SEXP a)
protect x = liftProtect Prelude.True R.protect (return x)

-- Lift an action and protect value.
liftProtect :: Bool -- if value should be registered in stack
            -> (a -> IO b) -> IO a -> R s b
liftProtect p r f = R . ReaderT $ \cnt -> mask_ $ do
   z <- r =<< f
   when p $ modifyIORef' cnt succ
   return z

-- | A type wrapper that represents that the value is not protected in R region.
-- This wrapper is used to return a @SEXP@ values from the region to the parent one
-- It's not possible to 'duplicate' a value to the parent without a negative impact on
-- performance, so user will have to use this method. 
--
-- The guaranteed lifetime of 'UnsafeValues' is a time before next R garbage collection,
-- in other words before the next allocation in R code occurs. So the following code is 
-- safe:
--
-- protect <=< runRegion $ do
--    mapM_ (\i -> [r| i_hs + 1|]) [1..10]
-- 
--
-- If you need better safety proterties concider using 'Language.R.GC.RVal'.
newtype UnsafeValue a = UnsafeValue a

-- | Use an unprotected value.
unsafeUseValue :: UnsafeValue a -> (a -> b) -> b
unsafeUseValue (UnsafeValue a) f = f a

mkUnsafe :: a -> UnsafeValue a
mkUnsafe = UnsafeValue

