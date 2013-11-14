-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R-interpreter
-- in the background thread and interact with it.
{-# LANGUAGE DataKinds #-}
module Language.R.Interpreter where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import           Foreign.C.String

import Control.Exception ( bracket )
import Control.Monad ( forM_, when )

import Foreign ( poke, pokeElemOff, allocaArray )
import System.Environment ( getProgName, lookupEnv )
import System.Process     ( readProcess )
import System.SetEnv

data RRequest   = ReqParse FilePath (R.SEXP (R.Vector (R.SEXP R.Any)) -> IO ())
data RError     = RError

data RConfig = RConfig
       { rProgName :: Maybe String
       , rParams   :: [String]
       }

populateEnv :: IO ()
populateEnv = do
    mh <- lookupEnv "R_HOME"
    when (mh == Nothing) $
      setEnv "R_HOME" =<< fmap (head . lines) (readProcess "R" ["RHOME"] "")

initializeR :: Maybe RConfig -> IO ()
initializeR Nothing =
    initializeR (Just $ RConfig Nothing ["--vanilla","--silent","--quiet"])
initializeR (Just (RConfig nm prm)) = do
    populateEnv
    pn <- case nm of
            Nothing -> getProgName
            Just x  -> return x
    -- TODO: it's possible to populate with other options
    allocaArray (length prm+1) $ \a -> do
        sv1 <- newCString pn
        pokeElemOff a 0 sv1
        forM_ (zip prm [1..]) $ \(v,i) -> do
            pokeElemOff a i =<< newCString v
        R.initEmbeddedR (length prm+1) a
    poke R.rInteractive 0

deinitializeR :: IO ()
deinitializeR = R.endEmbeddedR 0

-- | Initialize R runtime in the main thread and automatically
-- deinitilize in on exit from the function scope.
withR :: Maybe RConfig -- ^ R configuration options
      -> IO a
      -> IO a
withR cfg = bracket (initializeR cfg) (const deinitializeR) . const
