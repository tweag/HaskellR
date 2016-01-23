-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Interaction with an instance of R. The interface in this module allows for
-- instantiating an arbitrary number of concurrent R sessions, even though
-- currently the R library only allows for one global instance, for forward
-- compatibility.
--
-- The 'R' monad defined here serves to give static guarantees that an instance
-- is only ever used after it has been initialized and before it is finalized.
-- Doing otherwise should result in a type error. This is done in the same way
-- that the 'Control.Monad.ST' monad encapsulates side effects: by assigning
-- a rank-2 type to the only run function for the monad.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Language.R.Instance
  ( -- * The R monad
    R
  , runRegion
  -- * R instance creation
  , Config(..)
  , defaultConfig
  , withEmbeddedR
  , initialize
  , finalize
  ) where

import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.R.Class
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Monoid
import           Data.Default.Class (Default(..))
import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Foreign.R.EventLoop as R
import           Foreign.C.String
import           Language.R.Globals

import Control.Applicative
import Control.Concurrent.MVar
    ( newMVar
    , withMVar
    , MVar
    )
import Control.DeepSeq ( NFData, deepseq )
import Control.Exception
    ( bracket
    , bracket_
    , uninterruptibleMask_
    )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )
import Control.Monad.Reader
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Foreign
    ( Ptr
    , allocaArray
    )
import Foreign.C.Types ( CInt(..) )
import Foreign.Storable (Storable(..))
import System.Environment ( getProgName, lookupEnv )
import System.IO.Unsafe   ( unsafePerformIO )
import System.Process     ( readProcess )
import System.SetEnv
#ifdef H_ARCH_UNIX
import Control.Exception ( onException )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Resource
#endif
import Prelude

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R s a = R { unR :: ReaderT (IORef Int) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadMask, MonadThrow)

instance PrimMonad (R s) where
  type PrimState (R s) = s
  primitive f = R $ lift $ unsafeSTToIO $ primitive f

instance MonadR (R s) where
  io m = R $ ReaderT $ \_ -> m
  acquire s = R $ ReaderT $ \cnt -> uninterruptibleMask_ $ do
    x <- R.release <$> R.protect s
    modifyIORef' cnt succ
    return x
  unsafeToIO m = do
    ref <- newIORef 0
    runReaderT (unR m) ref

-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance. This is typically called at the
-- very beginning of the @main@ function of the program.
--
-- > main = withEmbeddedR $ do {...}
--
-- Note that R does not currently support reinitialization after finalization,
-- so this function should be called only once during the lifetime of the
-- program (see @src/unix/system.c:Rf_initialize()@ in the R source code).
withEmbeddedR :: Config -> IO a -> IO a
withEmbeddedR config = bracket_ (initialize config) finalize

-- | Run an R action in the global R instance from the IO monad. This action
-- provides no static guarantees that the R instance was indeed initialized and
-- has not yet been finalized. Make sure to call it within the scope of
-- `withEmbeddedR`.
--
-- @runRegion m@ fully evaluates the result of action @m@, to ensure that no
-- thunks hold onto resources in a way that would extrude the scope of the
-- region. This means that the result must be first-order data (i.e. not
-- a function).
runRegion :: NFData a => (forall s . R s a) -> IO a
runRegion r =
  bracket (newIORef 0)
          (R.unprotect <=< readIORef)
          (\d -> do
             x <- runReaderT (unR r) d
             x `deepseq` return x)

-- | Configuration options for the R runtime. Configurations form monoids, so
-- arguments can be accumulated left-to-right through monoidal composition.
data Config = Config
  { -- | Program name. If 'Nothing' then the value of 'getProgName' will be
    -- used.
    configProgName :: Last String
    -- | Command-line arguments.
  , configArgs :: [String]

    -- | Set to 'True' if you're happy to let R install its own signal handlers
    -- during initialization.
  , configSignalHandlers :: Last Bool
  }

instance Default Config where
  def = defaultConfig

instance Monoid Config where
  mempty = defaultConfig
  mappend cfg1 cfg2 = Config
      { configProgName = configProgName cfg1 <> configProgName cfg2
      , configArgs = configArgs cfg1 <> configArgs cfg2
      , configSignalHandlers =  configSignalHandlers cfg1 <> configSignalHandlers cfg2
      }

-- | Default argument to pass to 'initialize'.
defaultConfig :: Config
defaultConfig = Config (Last Nothing) ["--vanilla", "--silent"] (Last (Just False))

-- | Populate environment with @R_HOME@ variable if it does not exist.
populateEnv :: IO ()
populateEnv = do
    mh <- lookupEnv "R_HOME"
    when (mh == Nothing) $
      setEnv "R_HOME" =<< fmap (head . lines) (readProcess "R" ["-e","cat(R.home())","--quiet","--slave"] "")

-- | A static address that survives GHCi reloadings which indicates
-- whether R has been initialized.
foreign import ccall "missing_r.h &isRInitialized" isRInitializedPtr :: Ptr CInt

-- | Allocate and initialize a new array of elements.
newCArray :: Storable a
          => [a]                                  -- ^ Array elements
          -> (Ptr a -> IO r)                      -- ^ Continuation
          -> IO r
newCArray xs k =
    allocaArray (length xs) $ \ptr -> do
      zipWithM_ (pokeElemOff ptr) [0..] xs
      k ptr

-- | An MVar to make an atomic step of checking whether R is initialized and
-- initializing it if needed.
initLock :: MVar ()
initLock = unsafePerformIO $ newMVar ()
{-# NOINLINE initLock #-}

-- Note [Concurrent initialization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- In 'initialize' we check a first time if R is initialized. This test is fast
-- since it happens without taking an MVar. If R needs initialization, after
-- taking the MVar we check again if R is initialized to avoid concurrent
-- threads from initializing R multiple times. The user is not expected to call
-- initialize multiple times concurrently, but there is nothing stopping the
-- compiler from doing so when compiling quasiquotes.

-- | Create a new embedded instance of the R interpreter. Only works from the
-- main thread of the program. That is, from the same thread of execution that
-- the program's @main@ function is running on. In GHCi, use @-fno-ghci-sandbox@
-- to achieve this.
initialize :: Config -> IO ()
initialize Config{..} = do
#ifdef H_ARCH_UNIX
#ifdef H_ARCH_UNIX_DARWIN
    -- NOTE: OS X does not allow removing the stack size limit completely,
    -- instead forcing a hard limit of just under 64MB.
    let stackLimit = ResourceLimit 67104768
#else
    let stackLimit = ResourceLimitUnknown
#endif
    setResourceLimit ResourceStackSize (ResourceLimits stackLimit stackLimit)
      `onException` (hPutStrLn stderr $
                       "Language.R.Interpreter: "
                       ++ "Cannot increase stack size limit."
                       ++ "Try increasing your stack size limit manually:"
#ifdef H_ARCH_UNIX_DARWIN
                       ++ "$ launchctl limit stack 67104768"
                       ++ "$ ulimit -s 65532"
#else
                       ++ "$ ulimit -s unlimited"
#endif
                    )
#endif
    initialized <- fmap (==1) $ peek isRInitializedPtr
    -- See note [Concurrent initialization]
    unless initialized $ withMVar initLock $ const $ do
      initialized2 <- fmap (==1) $ peek isRInitializedPtr
      unless initialized2 $ mdo
        -- Grab addresses of R global variables
        pokeRVariables
          ( R.baseEnv
          , R.emptyEnv
          , R.globalEnv
          , R.nilValue
          , R.unboundValue
          , R.missingArg
          , R.isRInteractive
          , R.inputHandlers
          , R.signalHandlers
          )
        populateEnv
        args <- (:) <$> maybe getProgName return (getLast configProgName)
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        unless (maybe False id $ getLast configSignalHandlers) $
          poke signalHandlersPtr 0
        newCArray argv $ R.initEmbeddedR argc
        poke isRInteractive 0
        poke isRInitializedPtr 1

-- | Finalize an R instance.
finalize :: IO ()
finalize = do
    R.endEmbeddedR 0
    poke isRInitializedPtr 0
