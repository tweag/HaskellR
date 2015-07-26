-- |
-- Copyright: (C) 2015 Tweag I/O Limited.
--
-- Helper module for writing event loops that mesh with R events.
--
-- Events in R are dispatched from a number of file descriptors. The R runtime
-- maintains a list of "input handlers", essentially a set of file descriptors
-- together with callbacks for each one, invoked whenever the file descriptor
-- becomes available for reading. This module exports functions for dispatching
-- on both R events and Haskell events simultaneously, using "GHC.Event", which
-- is based on epoll/kqueue/poll under the hood for efficient and scalable event
-- dispatching.
--
-- Event dispatching and processing is in particular necessary for R's GUI to be
-- responsive. For a consistent user experience, you should arrange for all GUI
-- related events to be dispatched from a single thread, ideally the program's
-- main thread. In fact on some platforms, most notably OS X (darwin), you
-- /must/ use the main thread.
--
-- Event loops can be constructed in one of two ways:
--
--   1. 'eventLoopPoll', which uses GHC's @poll(2)@ (and related syscalls) based
--   efficient and scalable mechanisms for event dispatch;
--
--   2. 'eventLoopSelect', which uses R's @select(2)@ based mechasism.
--
-- __NOTE:__ in GHC 7.8 and 7.10, 'eventLoopPoll' is currently unusable, due to
-- a number of functions from the event API not being exported like they were
-- previously.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Language.R.Event
  ( forIH
  , forIH_
  , registerREvents
  , eventLoopPoll
  , eventLoopSelect
  , refresh
  ) where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.R.Class
import Data.Maybe (catMaybes)
import qualified Foreign.R.EventLoop as R
import qualified GHC.Event as Event
import Language.R.Globals (inputHandlers)
import Foreign (FunPtr, Ptr, nullPtr, peek)
import Prelude -- Silence AMP warning.

-- | Iterate over each input handler in a chain.
forIH :: Ptr R.InputHandler -> (R.InputHandler -> IO a) -> IO [a]
forIH ihptr f
  | ihptr == nullPtr = return []
  | otherwise = do
    ih <- peek ihptr
    (:) <$> f ih <*> forIH (R.inputHandlerNext ih) f

-- | Variant of 'forIH' that throws away the result.
forIH_ :: Ptr R.InputHandler -> (R.InputHandler -> IO ()) -> IO ()
forIH_ ihptr f
  | ihptr == nullPtr = return ()
  | otherwise = do
    ih <- peek ihptr
    f ih
    forIH_ (R.inputHandlerNext ih) f

foreign import ccall "dynamic" invokeIO :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" invokeCallback :: FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

-- | Register all R input handlers with the given event manager. Set an alarm to
-- process polled events if @R_wait_usec@ is non-zero. Returns keys useful for
-- unregistering input handlers.
registerREvents
  :: MonadR m
  => Event.EventManager
  -> m ([Event.FdKey], Maybe Event.TimeoutKey)
registerREvents emgr = io $ do
    tmgr <- Event.getSystemTimerManager
    fdkeys <- forIH inputHandlers $ \R.InputHandler{..} -> do
      let action _ _ = invokeCallback inputHandlerCallback inputHandlerUserData
      case 0 < inputHandlerActive of
        True ->
#if MIN_VERSION_base(4,8,0)
          Just <$> Event.registerFd emgr action inputHandlerFD Event.evtRead Event.MultiShot
#else
          Just <$> Event.registerFd emgr action inputHandlerFD Event.evtRead
#endif
        False -> return Nothing
    usecs <- peek R.pollingPeriod
    gusecs <- peek R.graphicsPollingPeriod
    let eusecs
          | usecs == 0 && gusecs == 0 = 10000
          | usecs == 0 || gusecs == 0 = max usecs gusecs
          | otherwise = min usecs gusecs
    mbtkey <- case 0 < eusecs of
      True -> do
        let action = do
              peek R.polledEvents >>= invokeIO
              peek R.graphicsPolledEvents >>= invokeIO
        Just <$> Event.registerTimeout tmgr (fromIntegral usecs) action
      False -> return Nothing
    return (catMaybes fdkeys, mbtkey)

-- | Process events in a loop. Uses a new GHC event manager under the hood. This
-- function should be called from the main thread. It never returns.
--
-- Currently unimplemented.
eventLoopPoll :: MonadR m => m ()
eventLoopPoll = error "Unimplemented."

-- | Process events in a loop. Uses R's @select()@ mechanism under the hood.
-- This function should be called from the main thread. It never returns.
eventLoopSelect :: MonadR m => m ()
eventLoopSelect =
    io $ forever $ do
      usecs <- peek R.pollingPeriod
      gusecs <- peek R.graphicsPollingPeriod
      let eusecs
            | usecs == 0 && gusecs == 0 = 10000
            | usecs == 0 || gusecs == 0 = max usecs gusecs
            | otherwise = min usecs gusecs
      R.checkActivity eusecs 1 >>=
        R.runHandlers inputHandlers

-- | Manually trigger processing all pending events. Useful when at an
-- interactive prompt and no event loop is running.
refresh :: MonadR m => m ()
refresh = io $ R.checkActivity 0 1 >>= R.runHandlers inputHandlers
