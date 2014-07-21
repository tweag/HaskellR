-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Utilities to run R interpreter within a haskell program.
-- This module starts a dedicated bound in order to communicate with R Runtime.
--
-- The requirements for a dedicated bound thread is appeared from following facts
--
--    *  R supports only single threaded executioni
--
--    *  All foreign calls to R have to be executed from the same OS thread.
--
-- Second requirement is not hard unless graphics functions are used and R stack
-- protection mechanism is disabled. On OS X platform R interpreter should be
-- running on the main OS thread.
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Foreign.R.Runner
  ( -- * Data types
    Config(..)
  , defaultConfig
  -- * R instance creation
  , initialize
  , finalize
  , unsafeRunInRThread
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
  , getPostToCurrentRThread
  ) where

import           Control.Concurrent.OSThread
import qualified Foreign.R.Internal as R
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
    , finally
    , throwIO
    , try
    )
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
import System.IO.Unsafe   ( unsafePerformIO )
import System.Mem.Weak ( mkWeakPtr, deRefWeak)
import System.Process     ( readProcess )
import System.SetEnv
#ifdef H_ARCH_UNIX
import Control.Exception ( onException )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Resource
#endif

-- | Configuration options for R runtime.
data Config = Config
    { configProgName :: Maybe String    -- ^ Program name. If 'Nothing' then
                                        -- value of 'getProgName' will be used.
    , configArgs     :: [String]        -- ^ Command-line arguments.
    }

---------------------------------------------------------------------------------
-- Utilities to run R
---------------------------------------------------------------------------------

-- | Default configuration.
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
--
-- This function do the following:
--
--    1 initlializes R Runtime;
--
--    2 populates all global variables to workaround  $ghci-bug;
--
--    3 starts an GUI thread that processes GUI events on a timely manner
--
--    4 starts an R interpreter thread that serializes calls to R.
--
-- NOTE: This function is idempotent so it's possible to call it multiple times.
initialize :: Config
           -> IO ()
initialize Config{..} = do
    initialized <- fmap (==1) $ peek isRInitializedPtr
    unless initialized $ mdo
      -- Grab addresses of R global variables
      pokeRVariables
        ( R.globalEnv, R.baseEnv, R.nilValue, R.unboundValue, R.missingArg
        , R.rInteractive, R.rInputHandlers
        )
      startRThread eventLoopThread
      eventLoopThread <- forkIO $ forever $ do
        threadDelay 30000
#ifdef H_ARCH_WINDOWS
        unsafeRunInRThread R.processEvents
#else
        unsafeRunInRThread $
          R.processGUIEventsUnix rInputHandlersPtr
#endif
      unsafeRunInRThread $ do
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initUnlimitedEmbeddedR argc
        poke rInteractive 0
        poke isRInitializedPtr 1

-- | Finalize an R instance.
--
-- NOTE: currently it's not possible to reinitialize an R, so once 'finalize' is
-- called, it's not possible to 'initialize' R runtime once again.
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
--
-- $ghci-bug
-- The main reason to have all R constants referenced with a StablePtr
-- is that variables in shared libraries are linked incorrectly by GHCi with
-- loaded code.
--
-- The workaround is to grab all variables in the ghci session for the loaded
-- code to use them, that is currently done by the H.ghci script.
--
-- Upstream ticket: <https://ghc.haskell.org/trac/ghc/ticket/8549#ticket>

type RVariables =
    ( Ptr (R.SEXP R.Env)
    , Ptr (R.SEXP R.Env)
    , Ptr (R.SEXP R.Nil)
    , Ptr (R.SEXP R.Symbol)
    , Ptr (R.SEXP R.Symbol)
    , Ptr CInt
    , Ptr (Ptr ())
    )

-- | Stores R variables in a static location. This makes the variables'
-- addresses accesible after reloading in GHCi.
pokeRVariables :: RVariables -> IO ()
pokeRVariables = poke rVariables <=< newStablePtr

-- | Retrieves R variables.
peekRVariables :: RVariables
peekRVariables = unsafePerformIO $ peek rVariables >>= deRefStablePtr

(  globalEnvPtr
 , baseEnvPtr
 , nilValuePtr
 , unboundValuePtr
 , missingArgPtr
 , rInteractive
 , rInputHandlersPtr
 ) = peekRVariables


foreign import ccall "missing_r.h &" rVariables :: Ptr (StablePtr RVariables)
