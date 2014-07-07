-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- The 'R' monad defined here serves to give static guarantees that an instance
-- is only ever used after it has been initialized and before it is finalized.
-- Doing otherwise should result in a type error. This is done in the same way
-- that the 'Control.Monad.ST' monad encapsulates side effects: by assigning
-- a rank-2 type to the only run function for the monad.
--
-- This module is intended to be imported qualified.
{-# LANGUAGE Rank2Types #-}
module Control.Monad.R
  ( -- * The R monad
    R(..)
  , runR
    -- * Regions
    -- $regions
  , runRegion
  , protectRegion
  , protectSomeRegion
  , unsafeRunRegion
  , protect
  , liftProtect
  , liftProtectSome

    -- * Monad modificators
  , unsafeRToIO
  , unsafeIOToR
  , unsafeRunInRThread
  , unsafePerformR
  , Config(..)
  , defaultConfig
  -- * R instance creation
  , initialize
  -- * Monad R
  , MonadR
  , io
  -- * R global constants
  -- $ghci-bug
  , pokeRVariables
  , globalEnvPtr
  , baseEnvPtr
  , nilValuePtr
  , unboundValuePtr
  , missingArgPtr
  , rInteractive
  , rInputHandlersPtr
  ) where

import           Foreign.R.Runner
import qualified Foreign.R.Internal as R
import           Control.Monad.R.Class


import Control.Applicative
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, mask_ )
import Control.DeepSeq
import Control.Exception ( bracket, bracket_, evaluate )
import Control.Monad.Reader
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s a = R { unR :: ReaderT (IORef Int) IO a }
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative)

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
runR :: Config -> (forall s. R s a) -> IO a
runR config r = bracket_ (initialize config) finalize (internalRunRegion r)

-- | Run a R code in a region. See $regions
--
-- This function creates a nested region and makes all objects inside a regions
-- as free.
--
-- 'NFData' constraint is used to prevent from leaks of any unevaluated objects
-- through the pure values.
--
-- 'SEXP' from a different regions can not be shared.
runRegion :: NFData a => (forall s. R s a) -> R s' a
runRegion = R . ReaderT . const . unsafeRunRegion

protectRegion :: (forall s . R s (R.SEXP a)) -> R s' (R.SEXP a)
protectRegion = R . ReaderT . const . internalRunRegion

protectSomeRegion :: (forall s . R s R.SomeSEXP) -> R s' R.SomeSEXP
protectSomeRegion = liftProtectSome . internalRunRegion

-- | Evaluate an region, in IO monad.
unsafeRunRegion :: NFData a => (forall s.R s a) -> IO a
unsafeRunRegion f = internalRunRegion (unsafeIOToR . evaluate . force  =<< f)

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
-- No unprotection is called on the return values.
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
protect x = liftProtect (return x)

-- | Lift an action that creates an SEXP value and protect it inside a region.
liftProtect :: IO (R.SEXP a) -> R s (R.SEXP a)
liftProtect f = mask_ $ R . ReaderT $ \cnt -> do
   z <- R.protect =<< f
   modifyIORef' cnt succ
   return z

-- | Lift an action that creates a SomeSEXP value and protect it inside a region.
liftProtectSome :: IO R.SomeSEXP -> R s R.SomeSEXP
liftProtectSome f = mask_ $ R . ReaderT $ \cnt -> do
   R.SomeSEXP z <- f
   k <- R.protect z
   modifyIORef' cnt succ
   return (R.SomeSEXP k)
