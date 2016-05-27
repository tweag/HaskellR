-- |
-- Copyright: (C) 2015 Tweag I/O Limited.
--
-- Bindings for @<R/R_ext/eventloop.h>@, for building event loops.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Foreign.R.EventLoop
  ( InputHandler(..)
  , inputHandlers
  , polledEvents
  , pollingPeriod
  , graphicsPolledEvents
  , graphicsPollingPeriod
  , checkActivity
  , runHandlers
  , addInputHandler
  , removeInputHandler
  ) where

import Control.Applicative
import Foreign (FunPtr, Ptr, Storable(..), castPtr)
import Foreign.C
import Foreign.Marshal.Utils (with)
import System.Posix.Types (Fd(..))
import Prelude -- Silence AMP warning.

#include <R_ext/eventloop.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | R input handler chain. Each input handler points to the next. This view of
-- input handlers is /shallow/, in the sense that the 'Storable' instance only
-- unmarshalls the first element in the chain at any one time. A shallow view
-- allows 'peek' and 'poke' to be inlinable.
data InputHandler = InputHandler
  { -- | The input handler callback.
    inputHandlerCallback :: FunPtr (Ptr () -> IO ())
    -- | Undocumented and currently unused.
  , inputHandlerActivity :: CInt
    -- | Whether this input handler is activated or deactivated.
  , inputHandlerActive :: CInt
    -- | The file descriptor ahssociated with this handler.
  , inputHandlerFD :: Fd
    -- | Callbacks can optionally be passed in arbitrary data.
  , inputHandlerUserData :: Ptr ()
    -- | The next input handler in the chain.
  , inputHandlerNext :: Ptr InputHandler
  } deriving (Eq, Show)

instance Storable InputHandler where
  sizeOf _ = #{size InputHandler}
  alignment _ = #{alignment InputHandler}
  peek hptr = InputHandler <$>
      #{peek InputHandler, handler} hptr <*>
      #{peek InputHandler, activity} hptr <*>
      #{peek InputHandler, active} hptr <*>
      (Fd <$> #{peek InputHandler, fileDescriptor} hptr) <*>
      #{peek InputHandler, userData} hptr <*>
      (castPtr <$> #{peek InputHandler, next} hptr)
  poke hptr InputHandler{..} = do
    #{poke InputHandler, handler} hptr inputHandlerCallback
    #{poke InputHandler, activity} hptr inputHandlerActivity
    #{poke InputHandler, active} hptr inputHandlerActive
    #{poke InputHandler, fileDescriptor} hptr (case inputHandlerFD of Fd fd -> fd)
    #{poke InputHandler, userData} hptr inputHandlerUserData
    #{poke InputHandler, next} hptr (castPtr inputHandlerNext)

-- | @R_PolledEvents@ global variable.
foreign import ccall "&R_PolledEvents" polledEvents :: Ptr (FunPtr (IO ()))

-- | @R_wait_usec@ global variable.
foreign import ccall "&R_wait_usec" pollingPeriod :: Ptr CInt

-- | @R_PolledEvents@ global variable.
foreign import ccall "&Rg_PolledEvents" graphicsPolledEvents :: Ptr (FunPtr (IO ()))

-- | @R_wait_usec@ global variable.
foreign import ccall "&Rg_wait_usec" graphicsPollingPeriod :: Ptr CInt

-- | Input handlers used in event loops.
foreign import ccall "&R_InputHandlers" inputHandlers :: Ptr (Ptr InputHandler)

data FdSet

foreign import ccall unsafe "R_checkActivity" checkActivity
  :: CInt
  -> CInt
  -> IO (Ptr FdSet)

foreign import ccall "R_runHandlers" runHandlers
  :: Ptr InputHandler
  -> Ptr FdSet
  -> IO ()

foreign import ccall "addInputHandler" addInputHandler_
  :: Ptr InputHandler
  -> Fd
  -> FunPtr (Ptr () -> IO ())
  -> CInt
  -> IO (Ptr InputHandler)

-- | Create and register a new 'InputHandler'. The given file descriptor should
-- be open in non-blocking read mode. Make sure to dispose of the callback using
-- 'freeHaskellFunPtr' after calling 'removeInputHandler' where appropriate.
addInputHandler
  :: Ptr InputHandler
  -> Fd
  -> FunPtr (Ptr () -> IO ())
  -> Int
  -> IO (Ptr InputHandler)
addInputHandler ihptr fd f activity = do
    addInputHandler_ ihptr fd f (fromIntegral activity)

foreign import ccall "removeInputHandler" removeInputHandler_
  :: Ptr (Ptr InputHandler)
  -> Ptr InputHandler
  -> IO CInt

-- | Remove an input handler from an input handler chain. Returns 'True' if the
-- handler was successfully removed, 'False' otherwise.
removeInputHandler :: Ptr InputHandler -> Ptr InputHandler -> IO Bool
removeInputHandler handlers ih =
    with handlers $ \handlersptr -> do
      rc <- removeInputHandler_ handlersptr ih
      case rc of
        0 -> return $ False
        1 -> return $ True
        _ -> error "removeInputHandler: unexpected result."
