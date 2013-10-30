-- |
-- Copyright: (C) 2013, Amgen, Inc.
--
-- This module provides datatype for representing Haskell
-- modules in Raskell.

module H.Module
  where

import H.Value
import Text.PrettyPrint ( Doc, ($$), (<+>) )
import qualified Text.PrettyPrint as P

import qualified Language.R.Foreign.Internal as R

-- | Generic structure of the haskell module that is created from R module.
data RModule = RModule
      { modPackage   :: Maybe String
      , modName      :: String
      , modImports   :: [String]
      , modFunctions :: [RValue]
      }

-- | Create default module.
mkMod :: Maybe String -> String -> RModule
mkMod pkg name = RModule pkg name [] []

-- | Pretty print module.
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

-- | Translate R expresstion to the module.
translate :: R.SEXP -> RModule -> RModule
translate _ mod = mod
