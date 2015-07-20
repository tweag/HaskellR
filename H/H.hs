-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Version ( showVersion )
import System.Console.CmdArgs
import System.Exit (exitWith)
import System.Process

import qualified Paths_H

data H = H
    { configFiles :: [FilePath]
    , configInteractive  :: FilePath
    } deriving (Eq, Data, Typeable, Show)

cmdSpec :: H
cmdSpec = H
  { configFiles = def &= args  &= typ "-- [GHCi options]"
  , configInteractive  = "ghci" &= explicit &= name "interactive" &= help "Run interpreter" &= opt "ghci" &= typ "ghci" &= help "Set an alternative haskell interpreter."
  }
  &= program "H" &=
  help "H wrapper over ghci. " &=
  summary ("H version " ++ showVersion Paths_H.version ++
           "\nCopyright (C) 2013-2014 Amgen, Inc." ++
           "\nCopyright (C) 2015 Tweag I/O Limited.")
  -- TODO: add details clause

main :: IO ()
main = do
    config <- cmdArgs cmdSpec
    case config of
      H {configFiles, configInteractive} -> do
        cfg  <- Paths_H.getDataFileName "H.ghci"
        let argv = configFiles ++ ["-v0", "-ghci-script", cfg]
        (_,_,_,ph) <-
            createProcess (proc configInteractive argv)
            { std_in = Inherit
            , std_out = Inherit
            , delegate_ctlc = True
            }
        exitWith =<< waitForProcess ph
