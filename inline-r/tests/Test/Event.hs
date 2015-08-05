-- | Tests for "Language.R.Event".

{-# LANGUAGE CPP #-}
module Test.Event where

#ifndef mingw32_HOST_OS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Foreign (FunPtr, Ptr, freeHaskellFunPtr)
import qualified Foreign.R.EventLoop as R
import H.Prelude
import Language.R.Event
import System.IO (hClose, hPutStrLn)
import System.IO.Temp (withSystemTempFile)
import System.Posix.IO
  ( OpenMode(..)
  , OpenFileFlags(..)
  , closeFd
  , defaultFileFlags
  , openFd
  )
import System.Posix.Types (Fd)
#endif
import Test.Tasty
import Test.Tasty.HUnit

#ifndef mingw32_HOST_OS
foreign import ccall "wrapper" wrap
  :: (Ptr () -> IO ())
  -> IO (FunPtr (Ptr () -> IO ()))
#endif

tests :: TestTree
tests = testGroup "events"
#ifdef mingw32_HOST_OS
    []
#else
    [ testCase "addInputHandler increases handler count" $ do
        withReadFd $ \fd -> do
          f <- wrap $ \_ -> return ()
          ref1 <- newIORef (0 :: Int)
          forIH_ inputHandlers $ \_ -> modifyIORef' ref1 (+1)
          _ <- R.addInputHandler inputHandlers fd f 0
          ref2 <- newIORef 0
          forIH_ inputHandlers $ \_ -> modifyIORef' ref2 (+1)
          n1 <- readIORef ref1
          n2 <- readIORef ref2
          n1 @?= n2 - 1
          freeHaskellFunPtr f

    , testCase "removeInputHandler decreases handler count" $ do
        withReadFd $ \fd -> do
          f <- wrap $ \_ -> return ()
          ih <- R.addInputHandler inputHandlers fd f 0
          (@?= True) =<< R.removeInputHandler inputHandlers ih
          freeHaskellFunPtr f

    , testCase "file events (select)" $ do
        withReadFd $ \fd -> do
          ref <- newIORef False
          f <- wrap $ \_ -> writeIORef ref True
          _ <- R.addInputHandler inputHandlers fd f 0
          runRegion $ refresh
          (@?= True) =<< readIORef ref
          freeHaskellFunPtr f
-- XXX GHC bug: https://ghc.haskell.org/trac/ghc/ticket/10736
{-
    , testCase "file events (poll)" $ do
        withReadFd $ \fd -> do
          mv <- newEmptyMVar
          f <- wrap $ \_ -> putMVar mv ()
          _ <- R.addInputHandler inputHandlers fd f 0
          Just evtmgr <- getSystemEventManager
          runRegion $ void $ registerREvents evtmgr
          Just () <- timeout 1000000 $ takeMVar mv
          freeHaskellFunPtr f
-}
    ]
  where
    withReadFd :: (Fd -> IO ()) -> IO ()
    withReadFd action =
      withSystemTempFile "inline-r-" $ \path h -> do
        hPutStrLn h "hello"
        hClose h
        fd <- openFd path ReadOnly Nothing defaultFileFlags{ nonBlock = True }
        action fd
        closeFd fd
#endif
