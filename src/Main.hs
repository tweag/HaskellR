-- |
-- Module: Main
-- Copyright: (C) 2013, Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Main
  where

import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import qualified Paths_raskell as Paths_raskell

import           Compiler.Raskell.Module
import           Math.R.Interpreter
import qualified Math.R.Foreign.Internal as R


data Raskell = Raskell
       { raskellFiles :: [String]
       }
       deriving (Eq, Data, Typeable, Show)

raskell = Raskell
  { raskellFiles = def &= args &= typ "FILES/DIRS" }
  &=
  verbosity &=
  help "R to Haskell translator" &=
  -- TODO: use version info from cabal, check copyright notice
  summary ("Raskell v" ++ showVersion Paths_raskell.version ++ ", (C) 2013, Amgen, Inc.")
  -- TODO: add details clause

main = do
    action <- cmdArgs raskell
    case action of
        Raskell []  -> putStrLn "no input files"
        Raskell fls -> withRInterpret $ \ch -> do
            cls <- mapM (\fl -> parseFile ch fl (go fl)) fls
            mapM_ (\(x,y) -> putStrLn (x++":") >> putStrLn (show y)) cls
  where
    go fl x = do
        R.printValue x    -- TODO: remove or put under verbose
        return (fl, prettyModule $ translate x (mkMod Nothing "Test"))
