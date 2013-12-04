-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R interpreter in a background thread and
-- interact with it.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Language.R.Interpreter
  ( Config(..)
  , defaultConfig
  -- * Initialization
  , isInitialized
  , initialize
  , finalize
  -- * helpers
  , with
  , evaluateInInterpreterThread
  , postToInterpreterThread
  ) where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Foreign.R.Interface as R
import qualified Language.R as LR
import           Foreign.C.String

import Control.Applicative
import Control.Concurrent
    ( forkOS
    , forkIO
    , threadDelay
    , takeMVar
    , putMVar
    , newEmptyMVar
    , myThreadId
    )
import Control.Concurrent.Chan ( readChan, newChan, writeChan, Chan )
import Control.Exception ( bracket, catch, SomeException, throwTo )
import Control.Monad ( unless, when, zipWithM_, forever, void )

import Foreign ( Ptr, allocaArray, StablePtr, newStablePtr, deRefStablePtr )
import Foreign.C.Types ( CInt(..) )
import Foreign.Storable (Storable(..))
import System.Environment ( getProgName, lookupEnv )
import System.IO.Unsafe   ( unsafePerformIO )
import System.Process     ( readProcess )
import System.SetEnv

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

-- | A static address that survives GHCi reloadings.
foreign import ccall "missing_r.h &isRInitialized" isRInitializedPtr :: Ptr CInt

-- | Status of initialization.
isInitialized :: IO Bool
isInitialized = fmap (toEnum . fromIntegral) $ peek isRInitializedPtr

-- | Allocate and initialize a new array of elements.
newCArray :: Storable a
          => [a]                                  -- ^ Array elements
          -> (Ptr a -> IO r)                      -- ^ Continuation
          -> IO r
newCArray xs k =
    allocaArray (length xs) $ \ptr -> do
      zipWithM_ (pokeElemOff ptr) [0..] xs
      k ptr

-- | Initialize the R environment.
initialize :: Config
           -> IO ()
initialize Config{..} = do
    initialized <- isInitialized
    unless initialized $ do
      -- Grab addresses of R global variables
      LR.pokeRVariables
        ( R.globalEnv, R.baseEnv, R.nilValue, R.unboundValue, R.missingArg
        , R.rInteractive, R.rCStackLimit, R.rInputHandlers
        )
      startInterpreterThread
      evaluateInInterpreterThread $ do
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initEmbeddedR argc
        poke LR.rInteractive 0
        -- setting the stack limit seems to only be required in Windows
        poke LR.rCStackLimitPtr (-1)
        void $ forkIO $ forever $ do
          threadDelay 30000
#ifdef H_ARCH_WINDOWS
          evaluateInInterpreterThread R.processEvents
#else
          evaluateInInterpreterThread $
            R.processGUIEventsUnix LR.rInputHandlersPtr
#endif
        poke isRInitializedPtr 1

-- | Finalize R environment.
finalize :: IO ()
finalize = R.endEmbeddedR 0

-- | Properly acquire the R runtime, initializing R and ensuring that it is
-- finalized before returning.
with :: Config -- ^ R configuration options.
      -> IO a
      -> IO a
with cfg = bracket (initialize cfg) (const finalize) . const

-- | Starts the interpreter thread.
startInterpreterThread :: IO ()
startInterpreterThread = do
    chan <- newChan
    newStablePtr chan >>= poke interpreterChanPtr
    void $ forkOS $ forever $
      catch (readChan chan >>= id) $ \e -> const (return ()) (e :: SomeException)

-- | Posts a computation to perform in the interpreter thread.
--
-- Returns immediately without waiting for the action to be computed.
--
postToInterpreterThread :: IO () -> IO ()
postToInterpreterThread = writeChan interpreterChan
  where
    interpreterChan = unsafePerformIO $
      peek interpreterChanPtr >>= deRefStablePtr

-- | Evaluates a computation in the interpreter thread.
--
-- Waits until the computation is complete and returns back the result.
--
evaluateInInterpreterThread :: IO a -> IO a
evaluateInInterpreterThread action = do
    mv <- newEmptyMVar
    tid <- myThreadId
    postToInterpreterThread $ catch (action >>= putMVar mv) $ \e ->
      throwTo tid (e :: SomeException)
    takeMVar mv

-- | A static address that survives GHCi reloadings.
foreign import ccall "missing_r.h &interpreterChan" interpreterChanPtr :: Ptr (StablePtr (Chan (IO ())))
