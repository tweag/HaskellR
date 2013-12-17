-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable, CPP #-}
module Main where

import           H.Module
import qualified Language.R.Interpreter as R ( defaultConfig, with )
import           Language.R ( parseFile )

import           Control.Applicative
import           Control.Exception
import           Control.Monad ( void )
import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import           System.Exit (exitFailure)
import           System.Process

import qualified Paths_H
#ifdef H_ARCH_UNIX
import           System.Posix.Signals
#endif

data Config = Config
    { configFiles :: [String]
    , configGhci  :: Bool
    , configInteractive  :: Bool
    , configInteractiveCommand :: FilePath
    } deriving (Eq, Data, Typeable, Show)

cmdSpec :: Config
cmdSpec = Config
  { configFiles = def &= args &= typ "FILES/DIRS"
  , configGhci  = def &= explicit &= name "ghci" &= help "Prepare GHCI compatible output"
  , configInteractive  = def &= explicit &= name "interactive" &= help "Run interpreter"
  , configInteractiveCommand = "ghci"
  }
  &=
  verbosity &=
  help "R-to-Haskell translator." &=
  summary ("H version " ++ showVersion Paths_H.version ++
           "\nCopyright (C) 2013 Amgen, Inc.")
  -- TODO: add details clause

main :: IO ()
main = do
    config <- cmdArgs cmdSpec
    case config of
      Config {configFiles, configInteractive = True, configInteractiveCommand} -> do
        cfg <- Paths_H.getDataFileName "H.ghci"
        let argv = configFiles ++ ["-v0", "-ghci-script", cfg]
#if MIN_VERSION_process(1,2,0)
#ifdef H_ARCH_UNIX
        _ <- installHandler sigINT Ignore Nothing
        _ <- installHandler sigTERM Ignore Nothing
        _ <- installHandler sigQUIT Ignore Nothing
#endif
        (_,_,_,ph) <-
            createProcess (proc configInteractiveCommand argv)
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
            createProcess (proc configInteractiveCommand argv)
            { std_in = Inherit
            , std_out = Inherit
            }
#endif
        let loop = (void $ waitForProcess ph) `onException` (putStrLn "exception" >> loop)
        loop
        putStrLn "Bye!"
      Config {configFiles = []} -> do
        putStrLn "no input files"
        exitFailure
      Config {configFiles = [file], configGhci = True} -> R.with R.defaultConfig $ do
        print =<< parseFile file (\x ->
          prettyGhci <$> translate x (mkMod Nothing "Test"))
      Config {configFiles} -> R.with R.defaultConfig $ do
        ms <- mapM (`parseFile` (`translate` mkMod Nothing "Test")) configFiles
        mapM_ (\(x,y) -> putStrLn (x ++ ":") >> print y) $
          zip configFiles (map prettyModule ms)
