-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R-interpreter
-- in the background thread and interact with it.
{-# LANGUAGE DataKinds #-}
module Language.R.Interpreter where

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Foreign.R.Parse    as R
import           Foreign.C.String

import Control.Concurrent.Async ( async, cancel, link )
import Control.Concurrent.STM
  ( TChan
  , atomically
  , newTChanIO
  , newTChanIO
  , readTChan
  , writeTChan
  , newEmptyTMVarIO
  , takeTMVar
  , putTMVar )
import Control.Exception ( bracket )
import Control.Monad ( forever, forM_, void, when )

import Foreign ( castPtr, peek, poke, pokeElemOff, alloca, allocaArray )
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

-- | Run interpretator in background thread
withRInterpret :: (TChan RRequest -> IO a)  -- ^ actions to run
               -> IO a
withRInterpret f =
    bracket
      (do ch <- newTChanIO
          tid <- async $ interpret ch
          link tid
          return (ch, tid))
      (cancel . snd)
      (f . fst)

interpret :: TChan RRequest -> IO ()
interpret ch = bracket startEmbedded endEmbedded (const go)
  where
    startEmbedded = do
        pn <- getProgName
        -- TODO: it's possible to populate with other options
        allocaArray 3 $ \a -> do
            sv1 <- newCString pn
            poke a sv1
            sv2 <- newCString "--vanilla"
            pokeElemOff a 1 sv2
            sv3 <- newCString "--silent"
            pokeElemOff a 2 sv3
            R.initEmbeddedR 3 a
        poke R.rInteractive 0
    endEmbedded _ = void $ R.endEmbeddedR 0
    go = do
        rNil <- peek R.nilValue
        forever $ do
            req <- atomically $ readTChan ch
            case req of
              ReqParse str callback ->
                 withCString str $ \cstr ->
                   protect (R.mkString cstr) $ \tmp ->
                     alloca $ \status ->
                       protect (R.parseVector tmp (-1) status rNil) $ \e -> do
                       callback (castPtr e)

parseFile :: TChan RRequest -> FilePath -> (R.SEXP (R.Vector (R.SEXP R.Any)) -> IO a) -> IO a
parseFile ch fl f = do
    box <- newEmptyTMVarIO
    str <- readFile fl
    let clb sexp = do r <- f sexp
                      atomically $ putTMVar box r
    atomically $ writeTChan ch (ReqParse str clb)
    atomically $ takeTMVar box

protect :: IO (R.SEXP a) -> (R.SEXP a -> IO b) -> IO b
protect sexp f = do
   e <- sexp
   _ <- R.protect e
   x <- f e
   R.unprotect 1
   return x
