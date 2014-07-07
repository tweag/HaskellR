{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Control.Monad.R
  ( -- * The R monad
    R(..)
  , runR
    -- * Regions
    -- $regions
  , runRegion
  , protect
    -- * Monad modificators
  , unsafeRToIO
  , unsafeIOToR
  , unsafeRunInRThread
  , unsafePerformR
  ) where

import           Foreign.R.Runner
import qualified Foreign.R.Internal as R

import 		Control.Monad.Catch
#if MIN_VERSION_exceptions(0,6,0)
  	(MonadThrow, MonadCatch, MonadMask, mask_)
#elif MIN_VERSION_exceptions(0,4,0)
  	(MonadThrow, MonadCatch, mask_)
#else
  	(MonadCatch, mask_)
#endif

import Control.Applicative
import Control.Exception ( bracket, bracket_ )
import Control.DeepSeq
import Control.Monad.Reader
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s a = R { unR :: ReaderT (IORef Int) IO a }
#if MIN_VERSION_exceptions(0,6,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative, MonadReader (IORef Int))
#elif MIN_VERSION_exceptions(0,4,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadThrow, Applicative, MonadReader (IORef Int))
#else
  deriving (Monad, MonadIO, Functor, MonadCatch, Applicative, MonadReader (IORef Int))
#endif

-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance.
--
-- WARNING. due to the bug in R it's not possible to reinitialize R code, this
-- means that when this function will exit, then it will not be possible to
-- run it again or perform any R computaion.
--
-- WARNING. Return value is not forced, this means that it's may leak some
-- computations.
runR :: Config -> (forall s. R s a) -> IO a
runR config (R m) = bracket_ (initialize config) finalize (runReaderT m =<< newIORef 0)

-- | Run a R code in a region. See $regions
--
-- This function creates a nested region and makes all objects inside a regions
-- as free.
--
-- 'NFData' constraint is used to prevent from leaks of any unevaluated objects
-- through the pure values.
runRegion :: NFData a => (forall s. R s a) -> R s' a
runRegion f = R $ ReaderT $ \_ -> do 
   bracket (newIORef 0)
           (readIORef >=> R.unprotect)
	   (runReaderT (unR f))

-- | Run an R action in the global R instance from the IO monad. This action is
-- unsafe in the sense that use of it bypasses any static guarantees provided by
-- the R monad, in particular that the R instance was indeed initialized and has
-- not yet been finalized. It is a backdoor that should not normally be used.
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

-- $regions
-- TODO: documentation

protect :: R.SEXP a -> R s (R.SEXP a)
protect x = mask_ $ R . ReaderT $ \cnt -> do
   modifyIORef' cnt succ
   R.protect x
