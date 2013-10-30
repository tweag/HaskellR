-- |
-- Module: Compiler.Raskell.Module 
-- Copyright: (C) 2013, Amgen, Inc.
--
-- This module provides datatype for representing Haskell
-- modules in Raskell.
module Compiler.Raskell.Module
  where

import Compiler.Raskell.Value
import Text.PrettyPrint ( Doc, ($$), (<+>) )
import qualified Text.PrettyPrint as P

-- | Generic structure of the haskell module that is created
-- from R module
data RModule = RModule
      { modPackage   :: Maybe String
      , modName      :: String
      , modImports   :: [String]
      , modFunctions :: [RValue]
      }

prettyModule :: RModule -> Doc
prettyModule rmod = 
    P.text "module" <+> P.text modname                        $$ 
    P.nest 4 (P.text "where")                                 $$
    P.text ""                                                 $$                            
    P.vcat (map (\t -> P.text "import" <+> P.text t) imports) $$
    P.vcat (map P.text functions)
  where
    modname = modName rmod
    imports = modImports rmod
    functions = []
