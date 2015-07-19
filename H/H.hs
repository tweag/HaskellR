-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Exception
import           Control.Monad ( void )
import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import           System.Process

import qualified Paths_H
#ifdef H_ARCH_UNIX
import           System.Posix.Signals
#endif

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
           "\nCopyright (C) 2013-2014 Amgen, Inc.")
  -- TODO: add details clause

main :: IO ()
main = do
    config <- cmdArgs cmdSpec
    case config of
      H {configFiles, configInteractive} -> do
        cfg  <- Paths_H.getDataFileName "H.ghci"
        let argv = configFiles ++ ["-v0", "-ghci-script", cfg]
#if MIN_VERSION_process(1,2,0)
#ifdef H_ARCH_UNIX
        _ <- installHandler sigINT Ignore Nothing
        _ <- installHandler sigTERM Ignore Nothing
        _ <- installHandler sigQUIT Ignore Nothing
#endif
        (_,_,_,ph) <-
            createProcess (proc configInteractive argv)
            { std_in = Inherit
            , std_out = Inherit
            , delegate_ctlc = False
            }
#else
#ifdef H_ARCH_UNIX
        _ <- installHandler sigINT Ignore Nothing
        _ <- installHandler sigTERM Ignore Nothing
        _ <- installHandler sigQUIT Ignore Nothing
#endif
        (_,_,_,ph) <-
            createProcess (proc configInteractive argv)
            { std_in = Inherit
            , std_out = Inherit
            }
#endif
        let loop = (void $ waitForProcess ph) `onException` (putStrLn "exception" >> loop)
        loop
        putStrLn "Bye!"
