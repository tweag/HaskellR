-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable, CPP #-}
module Main where

import           Control.Applicative
import           Control.Monad ( void )
import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import           System.Process
import qualified Paths_H

import           H.Module
import           H.Repl
import           Language.R.Interpreter ( withR )
import           Language.R ( parseFile )

data Config = Config
    { configFiles :: [String]
    , configGhci  :: Bool
    , configRepl  :: Bool
    } deriving (Eq, Data, Typeable, Show)

cmdSpec :: Config
cmdSpec = Config
  { configFiles = def &= args &= typ "FILES/DIRS"
  , configGhci  = def &= explicit &= name "ghci" &= help "Prepare GHCI compatible output"
  , configRepl  = def &= explicit &= name "repl" &= help "Run interpreter" }
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
      Config {configFiles, configRepl = True} -> do
        cfg <- Paths_H.getDataFileName "H.ghci"
        (_,_,_,ph) <-
            createProcess (proc replCommand (configFiles ++ ["-v0","-ghci-script",cfg]))
            { std_in = Inherit
            , std_out = Inherit
            }
        void $ waitForProcess ph
      Config {configFiles = []} -> do
        putStrLn "no input files"  -- XXX: exitStatus with fail
      Config {configFiles = [file], configGhci = True} -> withR Nothing $ do
        print =<< parseFile file (\x ->
          prettyGhci <$> translate x (mkMod Nothing "Test"))
      Config {configFiles} -> withR Nothing $ do
        ms <- mapM (`parseFile` (`translate` mkMod Nothing "Test")) configFiles
        mapM_ (\(x,y) -> putStrLn (x ++ ":") >> print y) $
          zip configFiles (map prettyModule ms)
