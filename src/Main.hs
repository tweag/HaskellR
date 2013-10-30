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


data Raskell = Raskell
       deriving (Eq, Data, Typeable, Show)

raskell = Raskell
  &= 
  verbosity &=
  help "R to Haskell translator" &=
  -- TODO: use version info from cabal, check copyright notice
  summary ("Raskell v" ++ showVersion Paths_raskell.version ++ ", (C) 2013, Amgen, Inc.")
  -- TODO: add details clause

main = do
  action <- cmdArgs raskell
  case action of
      Raskell ->
          let dummyModule = RModule Nothing "Test" ["RPrelude"] []
          in putStrLn $ show $ prettyModule dummyModule

