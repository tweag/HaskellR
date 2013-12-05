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
  ) where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Language.R as LR
import           Foreign.C.String

import Control.Applicative
import Control.Exception ( bracket )
import Control.Monad ( unless, when, zipWithM_ )

import Foreign ( Ptr, allocaArray )
import Foreign.C.Types ( CInt(..) )
import Foreign.Storable (Storable(..))
import System.Environment ( getProgName, lookupEnv )
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
initialize Config{..} = isInitialized >>= (`unless` go)
  where
    go = do
        -- Grab addresses of R global variables
        LR.pokeRVariables ( R.globalEnv, R.baseEnv, R.nilValue, R.unboundValue
                          , R.missingArg, R.rInteractive
                          )
        populateEnv
        args <- (:) <$> maybe getProgName return configProgName
                    <*> pure configArgs
        argv <- mapM newCString args
        let argc = length argv
        newCArray argv $ R.initEmbeddedR argc
        poke LR.rInteractive 0
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
