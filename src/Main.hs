-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Main
  where

import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import qualified Paths_H

import           H.Module
import           Language.R.Interpreter
import qualified Language.R.Foreign.Internal as R


data Config = Config
    { configFiles :: [String]
    } deriving (Eq, Data, Typeable, Show)

cmdSpec = Config
  { configFiles = def &= args &= typ "FILES/DIRS" }
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
        Config []  -> putStrLn "no input files"
        Config fls -> withRInterpret $ \ch -> do
            cls <- mapM (\fl -> parseFile ch fl (go fl)) fls
            mapM_ (\(x,y) -> putStrLn (x ++ ":") >> print y) cls
  where
    go fl x = do
        R.printValue x    -- TODO: remove or put under verbose
        return (fl, prettyModule $ translate x (mkMod Nothing "Test"))
