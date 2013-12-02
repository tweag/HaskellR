-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R interpreter in a background thread and
-- interact with it.

{-# LANGUAGE DataKinds #-}

module Language.R.Interpreter
  ( Config(..)
  , defaultConfig
  -- * Initialization
  , isInitialized
  , initializeR
  , deinitializeR
  -- * helpers
  , withR
  , initializeConstants
  ) where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Language.R as LR
import           Foreign.C.String

import Control.Exception ( bracket )
import Control.Monad ( forM_, when, unless )

import Data.IORef

import Foreign ( poke, peek, pokeElemOff, allocaArray )
import System.Environment ( getProgName, lookupEnv )
import System.Process     ( readProcess )
import System.SetEnv
import System.IO.Unsafe ( unsafePerformIO )

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

-- | Contains status of initialization.
isInitialized :: IORef Bool
isInitialized = unsafePerformIO $ newIORef False

-- | Initializes R environment.
initializeR :: Config
            -> IO ()
initializeR Config{..} = readIORef isInitialized >>= flip unless inner
  where
    inner = do
        populateEnv
        argv0 <- maybe getProgName return configProgName
        allocaArray (length configArgs + 1) $ \a -> do
            sv0 <- newCString argv0
            pokeElemOff a 0 sv0
            forM_ (zip configArgs [1..]) $ \(v,i) -> do
                pokeElemOff a i =<< newCString v
            R.initEmbeddedR (length configArgs + 1) a
        poke R.rInteractive 0
        initializeConstants
        writeIORef isInitialized True

-- | Finalize R environment.
deinitializeR :: IO ()
deinitializeR = R.endEmbeddedR 0

-- | Properly acquire the R runtime, initializing R and ensuring that it is
-- finalized before returning.
withR :: Config -- ^ R configuration options.
      -> IO a
      -> IO a
withR cfg = bracket (initializeR cfg) (const deinitializeR) . const

-- | Initialize all R constants in haskell.
--
-- Required in compiled files due to GHCi linking bug.
initializeConstants :: IO ()
initializeConstants = do
    writeIORef LR.globalEnv =<< peek R.globalEnv
    writeIORef LR.nilValue =<< peek R.nilValue
    writeIORef LR.unboundValue =<< peek R.unboundValue
    writeIORef LR.baseEnv =<< peek R.baseEnv
