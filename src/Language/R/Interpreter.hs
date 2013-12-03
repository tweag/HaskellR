-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R interpreter in a background thread and
-- interact with it.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE DataKinds #-}

module Language.R.Interpreter
  ( Config(..)
  , defaultConfig
  -- * Initialization
  , isInitialized
  , initialize
  , finalize
  -- * helpers
  , with
  , initializeConstants
  ) where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Language.R as LR
import           Foreign.C.String

import Control.Applicative
import Control.Exception ( bracket )
import Control.Monad ( unless, when, zipWithM_ )

import Data.IORef

import Foreign ( Ptr, allocaArray )
import Foreign.Storable (Storable(..))
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

-- | Status of initialization.
isInitialized :: IORef Bool
isInitialized = unsafePerformIO $ newIORef False

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
initialize Config{..} = readIORef isInitialized >>= (`unless` go)
  where
    go = do
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initEmbeddedR argc
        poke R.rInteractive 0
        initializeConstants
        writeIORef isInitialized True

-- | Finalize R environment.
finalize :: IO ()
finalize = R.endEmbeddedR 0

-- | Properly acquire the R runtime, initializing R and ensuring that it is
-- finalized before returning.
with :: Config -- ^ R configuration options.
      -> IO a
      -> IO a
with cfg = bracket (initialize cfg) (const finalize) . const

-- | Initialize all R constants in haskell.
--
-- Required in compiled files due to GHCi linking bug.
initializeConstants :: IO ()
initializeConstants = do
    writeIORef LR.globalEnv =<< peek R.globalEnv
    writeIORef LR.nilValue =<< peek R.nilValue
    writeIORef LR.unboundValue =<< peek R.unboundValue
    writeIORef LR.baseEnv =<< peek R.baseEnv
