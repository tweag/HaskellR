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
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Language.R.Instance
  ( -- * The R monad
    R
  , runR
  , unsafeRToIO
  , unsafeRunInRThread
  , Config(..)
  , defaultConfig
  -- * R instance creation
  , initialize
  , getPostToCurrentRThread
  ) where

import           Control.Monad.R.Class
import           Control.Concurrent.OSThread
import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import           Foreign.C.String

import Control.Applicative
import Control.Concurrent
    ( ThreadId
    , forkIO
    , forkOS
    , isCurrentThreadBound
    , killThread
    , threadDelay
    )
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Concurrent.Chan ( readChan, newChan, writeChan, Chan )
import Control.Exception
    ( SomeException
    , AsyncException(ThreadKilled)
    , bracket_
    , finally
    , throwIO
    , try
    )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )
import Control.Monad.Reader

import Foreign
    ( Ptr
    , StablePtr
    , allocaArray
    , newStablePtr
    , deRefStablePtr
    , freeStablePtr
    , intPtrToPtr
    , castPtrToStablePtr
    , castStablePtrToPtr
    )
import Foreign.C.Types ( CInt(..) )
import Foreign.Storable (Storable(..))
import System.Environment ( getProgName, lookupEnv )
import System.Mem.Weak ( mkWeakPtr, deRefWeak)
import System.Process     ( readProcess )
import System.SetEnv
#ifdef H_ARCH_UNIX
import Control.Exception ( onException )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Resource
#endif

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R a = R { _unR :: IO a }
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative)


instance MonadR R where
  io m = R $ unsafeRunInRThread m

-- | Initialize a new instance of R, execute actions that interact with the
-- R instance and then finalize the instance.
runR :: Config -> R a -> IO a
runR config (R m) = bracket_ (initialize config) finalize m

-- | Run an R action in the global R instance from the IO monad. This action is
-- unsafe in the sense that use of it doesn't make sure that an R instance was
-- indeed initialized and has not yet been finalized. It is a backdoor that
-- should not normally be used.
unsafeRToIO :: R a -> IO a
unsafeRToIO (R m) = m

-- | Configuration options for R runtime.
data Config = Config
    { configProgName :: Maybe String    -- ^ Program name. If 'Nothing' then
                                        -- value of 'getProgName' will be used.
    , configArgs     :: [String]        -- ^ Command-line arguments.
    }

defaultConfig :: Config
defaultConfig = Config Nothing ["--vanilla", "--silent"]

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

-- | Create a new embedded instance of the R interpreter.
initialize :: Config
           -> IO ()
initialize Config{..} = do
    initialized <- fmap (==1) $ peek isRInitializedPtr
    unless initialized $ mdo
      -- Grab addresses of R global variables
      startRThread eventLoopThread
      eventLoopThread <- forkIO $ forever $ do
        threadDelay 30000
#ifdef H_ARCH_WINDOWS
        unsafeRunInRThread R.processEvents
#else
        unsafeRunInRThread $
          R.processGUIEventsUnix R.rInputHandlers
#endif
      unsafeRunInRThread $ do
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initUnlimitedEmbeddedR argc
        poke R.rInteractive 0
        poke isRInitializedPtr 1

-- | Finalize an R instance.
finalize :: IO ()
finalize = do
    mv <- newEmptyMVar
    postToRThread_ $ do
      R.endEmbeddedR 0
      stablePtr <- peek interpreterChanPtr
      poke interpreterChanPtr $ castPtrToStablePtr (intPtrToPtr 2)
      freeStablePtr stablePtr
      poke isRInitializedPtr 0
      putMVar mv ()
      throwIO ThreadKilled
    takeMVar mv

-- | Starts the R thread.
startRThread :: ThreadId -> IO ()
startRThread eventLoopThread = do
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
    chan <- newChan
    mv <- newEmptyMVar
    void $ forkOS $ do
      myOSThreadId >>= putMVar mv
      forever (join $ readChan chan) `finally` killThread eventLoopThread
    rOSThreadId <- takeMVar mv
    newStablePtr (rOSThreadId, chan) >>= poke interpreterChanPtr

-- | Runs a computation in the R interpreter thread.
--
-- This operation blocks until the computation completes if called from the R
-- thread. Otherwise, it does not block.
--
-- If R runtime is not initialized, the behavior of this call is undefined.
--
postToRThread_ :: IO () -> IO ()
postToRThread_ action = do
  stablePtr <- peek interpreterChanPtr
  if castStablePtrToPtr stablePtr == intPtrToPtr 2
  then error "postToRThread_: H is not initialized."
  else deRefStablePtr stablePtr >>= postToThisRThread_ action

-- | Returns a computation that behaves like 'postToRThread_'
-- if the current R instance is still alive when the computation is evaluated.
-- Otherwise, it does nothing.
--
getPostToCurrentRThread :: IO (IO () -> IO ())
getPostToCurrentRThread = do
    w <- peek interpreterChanPtr >>= deRefStablePtr >>= flip mkWeakPtr Nothing
    return $ \action ->
      deRefWeak w >>= maybe (return ()) (postToThisRThread_ action)

-- | Like 'postToRThread_' but runs the computation in the given
-- R instance.
postToThisRThread_ :: IO () -> (OSThreadId,Chan (IO ())) -> IO ()
postToThisRThread_ action (rOSThreadId, interpreterChan) = do
    tid <- myOSThreadId
    isBound <- isCurrentThreadBound
    if tid == rOSThreadId && isBound
      then action -- run the action here if we are the R thread.
      else writeChan interpreterChan action

-- | Evaluates a computation in the R interpreter thread.
--
-- Waits until the computation is complete and returns back the result.
--
-- The /unsafe/ prefix means that no verification is made that the R
-- thread is running.
--
unsafeRunInRThread :: IO a -> IO a
unsafeRunInRThread action = do
    mv <- newEmptyMVar
    postToRThread_ $ try action >>= putMVar mv
    takeMVar mv >>= either (throwIO :: SomeException -> IO a) return

-- | A static address that survives GHCi reloadings.
foreign import ccall "missing_r.h &interpreterChan" interpreterChanPtr :: Ptr (StablePtr (OSThreadId,Chan (IO ())))
