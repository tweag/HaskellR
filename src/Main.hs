-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           H.Module
import           Language.R.Interpreter ( withR )
import           Language.R ( parseFile )

import           Distribution.System (OS(..), buildOS)

import           Control.Applicative
import           Control.Monad ( void )
import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import           System.Exit (exitFailure)
import           System.Process

import qualified Paths_H


data Config = Config
    { configFiles :: [String]
    , configGhci  :: Bool
    , configRepl  :: Bool
    , configReplCommand :: FilePath
    } deriving (Eq, Data, Typeable, Show)

cmdSpec :: Config
cmdSpec = Config
  { configFiles = def &= args &= typ "FILES/DIRS"
  , configGhci  = def &= explicit &= name "ghci" &= help "Prepare GHCI compatible output"
  , configRepl  = def &= explicit &= name "repl" &= help "Run interpreter"
  , configReplCommand = case buildOS of
      Windows -> "ghcii.sh"
      _ -> "ghci"
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
      Config {configFiles, configRepl = True, configReplCommand} -> do
        cfg <- Paths_H.getDataFileName "H.ghci"
        let argv = configFiles ++ ["-v0", "-ghci-script", cfg]
        (_,_,_,ph) <-
            createProcess (proc configReplCommand argv)
            { std_in = Inherit
            , std_out = Inherit
            }
        void $ waitForProcess ph
      Config {configFiles = []} -> do
        putStrLn "no input files"
        exitFailure
      Config {configFiles = [file], configGhci = True} -> withR Nothing $ do
        print =<< parseFile file (\x ->
          prettyGhci <$> translate x (mkMod Nothing "Test"))
      Config {configFiles} -> withR Nothing $ do
        ms <- mapM (`parseFile` (`translate` mkMod Nothing "Test")) configFiles
        mapM_ (\(x,y) -> putStrLn (x ++ ":") >> print y) $
          zip configFiles (map prettyModule ms)
