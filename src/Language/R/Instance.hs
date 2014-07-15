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

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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
    , yield
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
#if MIN_VERSION_exceptions(0,6,0)
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )
#elif MIN_VERSION_exceptions(0,4,0)
import Control.Monad.Catch ( MonadCatch, MonadThrow )
#else
import Control.Monad.Catch ( MonadCatch )
#endif
import Control.Monad.Reader
import Data.IORef

import Foreign
    ( Ptr
    , StablePtr
    , allocaArray
    , newStablePtr
    , deRefStablePtr
    , freeStablePtr
    , nullPtr
    , castPtr
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
import System.Exit
#endif

-- | The 'R' monad, for sequencing actions interacting with a single instance of
-- the R interpreter, much as the 'IO' monad sequences actions interacting with
-- the real world. The 'R' monad embeds the 'IO' monad, so all 'IO' actions can
-- be lifted to 'R' actions.
newtype R a = R { _unR :: IO a }
#if MIN_VERSION_exceptions(0,6,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadMask, MonadThrow, Applicative)
#elif MIN_VERSION_exceptions(0,4,0)
  deriving (Monad, MonadIO, Functor, MonadCatch, MonadThrow, Applicative)
#else
  deriving (Monad, MonadIO, Functor, MonadCatch, Applicative)
#endif


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
-- 
-- Rules for the value:
-- 
--  * positive odd value means that R is initializing
--  * positive even means that R is initialized
--  * negative odd value means that R is deinitializing
--  * nagative even value means that R is deinitialized
--
foreign import ccall "missing_r.h &isRInitializedPtr" isRInitializedPtr :: Ptr (StablePtr (IORef Int))

-- | Allocate and initialize a new array of elements.
newCArray :: Storable a
          => [a]                                  -- ^ Array elements
          -> (Ptr a -> IO r)                      -- ^ Continuation
          -> IO r
newCArray xs k =
    allocaArray (length xs) $ \ptr -> do
      zipWithM_ (pokeElemOff ptr) [0..] xs
      k ptr

isRInitialized :: IORef Int
isRInitialized = unsafePerformIO $ do
  isCreated <- fmap ((==) nullPtr) (peek (castPtr isRInitializedPtr) :: IO (Ptr ()))
  if isCreated 
  then do i <- newIORef 0
          poke isRInitializedPtr =<< newStablePtr i
          return i
  else deRefStablePtr =<< peek isRInitializedPtr
{-# NOINLINE isRInitialized #-}

-- | Create a new embedded instance of the R interpreter.
initialize :: Config
           -> IO ()
initialize Config{..} = do
    join $ fix $ \loop -> atomicModifyIORef' isRInitialized $ \x ->
      case x of
        _ | x > 0 && even x -> (x, return ())               -- initialized   - skipping
        _ | x > 0           -> (x, yield >> join loop)      -- initializing in other thread - wait
        _ | even x          -> let n = (-x) + 1             -- deinitialized - initializing
                               in  (n, go n `onException` restore n)
        _                   -> (x, yield >> join loop)      -- deinitializing - wait
  where
    restore n = atomicModifyIORef' isRInitialized $ \x ->
      if x == n
      then (x - 1, ())
      else (x, ())                                          -- XXX: possible race
    go check = mdo
      -- Grab addresses of R global variables
      pokeRVariables
        ( R.globalEnv, R.baseEnv, R.nilValue, R.unboundValue, R.missingArg
        , R.rInteractive, R.rInputHandlers
        )
      chan <- startRThread eventLoopThread
      eventLoopThread <- forkIO $ forever $ do
        threadDelay 30000
#ifdef H_ARCH_WINDOWS
        unsafeRunInRThread R.processEvents
#else
        unsafeRunInRThread $
          R.processGUIEventsUnix rInputHandlersPtr
#endif
       -- here we inline a unsafeRunInRThread because we know that we are
       -- not in R Thread and we know that R thread are being initialized
       -- by ourselfes; so we are dropping all checks.
      mv <- newEmptyMVar
      writeChan chan $ putMVar mv <=< try $ do
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initUnlimitedEmbeddedR argc
        poke rInteractive 0
        join $ atomicModifyIORef' isRInitialized $ \x ->
          if x == check
          then (x+1, return ())
          else (x, putStrLn "PANIC! Race condition in R initialization." >> exitFailure)
      takeMVar mv >>= either (throwIO :: SomeException -> IO a) return


-- | Finalize an R instance.
finalize :: IO ()
finalize = do
    join $ fix $ \loop -> atomicModifyIORef' isRInitialized $ \x -> 
      case x of
        _ | x > 0 && even x -> ((-x) + 1, go ((-x) + 1)) -- initialized   - go
        _ | x > 0           -> (x, yield >> join loop)   -- initializing  - wait
        _ | even x          -> (x, return ())            -- deinitialized - skip
        _                   -> (x, yield >> join loop)   -- deinitializing - wait
  where
    go check = do
      mv <- newEmptyMVar
      let action = do
            R.endEmbeddedR 0
            peek interpreterChanPtr >>= freeStablePtr
            join $ atomicModifyIORef' isRInitialized $ \x ->
              if x == check
              then (x-1, do putMVar mv ()
                            throwIO ThreadKilled)
              else (x, putStrLn "PANIC! Race condition in R deinitialization." >> exitFailure)
      (_rOSThreadId, interpreterChan) <- peek interpreterChanPtr >>= deRefStablePtr
      writeChan interpreterChan action
      takeMVar mv

-- | Starts the R thread.
startRThread :: ThreadId -> IO (Chan (IO ()))
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
    return chan

-- | Runs a computation in the R interpreter thread.
--
-- This operation blocks until the computation completes if called from the R
-- thread. Otherwise, it does not block.
--
postToRThread_ :: IO () -> IO ()
postToRThread_ action = do
    tid <- myOSThreadId
    isBound <- isCurrentThreadBound
    (rOSThreadId, interpreterChan) <- peek interpreterChanPtr >>= deRefStablePtr
    if tid == rOSThreadId && isBound
      then do isInitialized <- fmap (liftA2 (&&) even (>0)) $ readIORef isRInitialized
              when isInitialized action -- run the action here if we are the R thread.
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
