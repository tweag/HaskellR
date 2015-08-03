-- | Tests for "Language.R.Event".

{-# LANGUAGE CPP #-}
module Test.Event where

#ifndef mingw32_HOST_OS
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign (FunPtr, Ptr, freeHaskellFunPtr)
import qualified Foreign.R.EventLoop as R
import H.Prelude
import System.IO (hClose, hPutStrLn)
import System.IO.Temp (withSystemTempFile)
import System.Posix.IO
  ( OpenMode(..)
  , OpenFileFlags(..)
  , closeFd
  , defaultFileFlags
  , openFd
  )
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
    [
#ifndef mingw32_HOST_OS
      testCase "file events (select)" $ do
        withSystemTempFile "inline-r-" $ \path h -> do
          hPutStrLn h "hello"
          hClose h
          fd <- openFd path ReadOnly Nothing defaultFileFlags{ nonBlock = True }
          ref <- newIORef False
          f <- wrap $ \_ -> writeIORef ref True
          ih <- R.addInputHandler inputHandlers fd f 0
          runRegion $ refresh
          (@?= True) =<< readIORef ref
          (@?= True) =<< R.removeInputHandler inputHandlers ih
          freeHaskellFunPtr f
          closeFd fd
#endif
    ]
