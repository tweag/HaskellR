-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides a way to run R-interpreter
-- in the background thread and interact with it.
module Language.R.Interpreter
  where

import Control.Concurrent.Async ( async, cancel, link )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, newTChanIO, readTChan, writeTChan
                              , TMVar, newEmptyTMVarIO, takeTMVar, putTMVar )
import Control.Exception ( bracket, evaluate )
import Control.Monad ( void, forever )

import Foreign ( poke, pokeElemOff, peek, alloca, allocaArray )
import Foreign.C ( newCString )
import System.Environment ( getProgName )

import qualified Foreign.R as R
import qualified Foreign.R.Embedded as R
import qualified Foreign.R.Parse    as R

data RRequest   =
        ReqParse String (R.SEXP -> IO ())
data RError     = RError

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
              ReqParse str callback -> do
                 protect (R.mkString str) $ \tmp ->
                    alloca $ \status ->
                       protect (R.parseVector tmp (-1) status rNil) $ \e -> do
                       callback e

parseFile :: TChan RRequest -> FilePath -> (R.SEXP -> IO a) -> IO a
parseFile ch fl f = do
    box <- newEmptyTMVarIO
    str <- readFile fl
    let clb sexp = do r <- f sexp
                      atomically $ putTMVar box r
    atomically $ writeTChan ch (ReqParse str clb)
    atomically $ takeTMVar box


protect :: IO R.SEXP -> (R.SEXP -> IO a) -> IO a
protect exp f = do
   e <- exp
   R.protect e
   x <- f e
   R.unprotect 1
   return x
