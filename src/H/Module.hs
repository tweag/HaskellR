-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides datatype for representing Haskell modules in H.

{-# LANGUAGE DataKinds #-}
module H.Module
  ( RModule(..)
  , mkMod
  , prettyGhci
  , prettyModule
  , translate
  ) where

import H.Internal.Prelude
import H.Value
import qualified Foreign.R as R

import Control.Applicative
import Control.Monad ( forM )
import qualified Data.Vector.Unboxed as U
import Foreign ( castPtr, peekElemOff )
import Foreign.C

import Text.PrettyPrint ( Doc, ($$), (<+>) )
import qualified Text.PrettyPrint as P


-- | Generic structure of the haskell module that is created from R module.
data RModule = RModule
      { modPackage   :: Maybe String
      , modName      :: String
      , modImports   :: [String]
      , modFunctions :: [Doc]
      }

-- | Create default module.
mkMod :: Maybe String -> String -> RModule
mkMod pkg modname =
    RModule pkg modname ["H.Prelude"] []

-- | Pretty print module.
prettyModule :: RModule -> Doc
prettyModule rmod =
    P.text "module" <+> P.text modname                        $$
    P.nest 4 (P.text "where")                                 $$
    P.text ""                                                 $$
    P.vcat (map (\t -> P.text "import" <+> P.text t) imports) $$
    P.vcat functions
  where
    modname = modName rmod
    imports = modImports rmod
    functions = modFunctions rmod

prettyGhci :: RModule -> Doc
prettyGhci rmod =
    (if null imports
      then P.empty
      else P.text ":m +" <+> P.hsep (map P.text imports))            $$
    P.vcat functions
  where
    imports = modImports rmod
    functions = modFunctions rmod

-- | Translate R expression to the module
translate :: SEXP (R.Vector (SEXP R.Any)) -> RModule -> IO RModule
translate x rmod = do
    -- XXX: currently we have hardcoded ghci but it's not right
    ls <- translate2ghci <$> emit <$> translate0 x
    return $ rmod{modFunctions = ls}

-- | Step0 translation on this step we are mapping R Structures to
-- the unityped Haskell values, without deep inspection of actions to
-- optimize/rewrite R language.
--
-- This is the only step where we will need interpreter
translate0 :: SEXP (R.Vector (SEXP R.Any)) -> IO [RValue]
translate0 x = do
    l <- R.length x
    -- TODO create hi-level wrapper
    forM [0..(l-1)] $ \i -> do
       e <- R.index x i
       translateValue e
  where
    translateValue :: SEXP a -> IO RValue
    translateValue y = do
        ty <- R.typeOf y
        case ty of
          R.Nil   -> return RNil
          R.Real  -> translateReal (castPtr y)
          R.Lang  -> translateLang (castPtr y)
          R.Symbol-> RVar  <$> translateSym (castPtr y)
          R.List  -> RList <$> translateList (castPtr y)
          _       -> unimplemented "translateValue"
    translateLang :: SEXP R.Lang -> IO RValue
    translateLang y = do
        vl <- translateSym =<< R.car y
        ls <- translateList =<< R.cdr y
        return $ RLang vl ls
    translateSym :: SEXP R.Symbol -> IO String
    translateSym y = do
        nm  <- R.char =<< R.symbolPrintName y
        peekCString nm         -- TODO: this is not correct (!)
    translateReal :: SEXP (R.Vector Double) -> IO RValue
    translateReal y = do
        l    <- R.length y
        cptr <- R.real y
        v <- U.generateM l (\i -> peekElemOff cptr i)
        return $ RReal v
    translateList :: SEXP R.List -> IO [RValue]
    translateList y = do
        ty <- R.typeOf y
        case ty of
          R.Nil  -> return []
          R.List -> do
            z  <- R.car y
            o  <- translateValue z
            os <- translateList =<< R.cdr y
            return $ o:os
          _ -> unimplemented "translateList"

-- | Translate a set of RValues into the Haskell code
emit :: [RValue] -> [RExpr]
emit = concatMap go
    -- XXX: we have to keep state and change it to track env, variables
    -- naming and such stuff but we don't want to do it from the start!
  where
    -- constants are not changing anything just ignoring
    -- XXX: if we can access to 'result of the previous statement'
    -- this is no longer the case
    go z@(RReal _) = [REConst z]
    -- assignment of the value
    go (RLang  "<-" [lhs,rhs]) =
        case rhs of
          RLang "function" _ -> [REFun lhs rhs]
          _                  -> [REAssign lhs rhs]
    -- XXX: this is just wrong we want to assign temporary name to the
    -- value
    go (RLang x z) = [RECall x z]
    go _ = unimplemented "emit"


-- | This is Ghci version of the last step of the translation
--
translate2ghci :: [RExpr] -> [Doc]
translate2ghci = concatMap go
  where
    go (REConst x)    = [P.text "toHVal $" <+> value x]
    go (REAssign _ _) = error "translate-ghci: Assign is not implemented yet"
    go (REFun _ _)    = error "translate-ghci: Fun is not implemented yet"
    go (RECall x y)   =
          [P.text "toHVal $" <+> fun  x y]
      --       | rhs == RLang "function"  = [name lhs ++"="++ fun rhs]

name :: RValue -> Doc
name (RVar x) = P.text x
name _ = error "incorrect variable"

fun :: RFunction -> [RValue] -> Doc
fun "+" [a,b] = P.text "rplus"  <+> P.parens (value a) <+> P.parens (value b)
fun "-" [a,b] = P.text "rminus" <+> P.parens (value a) <+> P.parens (value b)
fun "/" [a,b] = P.text "rfrac"  <+> P.parens (value a) <+> P.parens (value b)
fun "*" [a,b] = P.text "rmult"  <+> P.parens (value a) <+> P.parens (value b)
fun "(" [a]   = P.parens $ value a
fun "c" (a:as) =
    -- XXX: support all types
    -- XXX: extract most generic type
    case a of
        RReal _ -> P.parens $ P.text "mkSEXP" <+> P.text "$"
                           <+> P.parens ( P.text (show $ extractDouble (a:as))
                                        <+> P.text "::[Double]"
                                        )
          where
            extractDouble :: [RValue] -> [Double]
            extractDouble = concatMap go
              where
                go (RReal x) = U.toList x
                go _         = []
        _ -> unimplemented "fun"
fun x _       = error $ "fun: function '" ++ x ++ "' is  unsupported:"

value :: RValue -> Doc
value y@(RVar _) = name y
value (RLang x y) = fun x y
value (RReal v)
  | U.length v == 1 = P.parens $ P.text "mkSEXP" <+> P.parens (P.text (show $ U.head v) <+> P.text "::Double")
--value y@(RReal x) = "(mkRTDouble " ++ (show $ U.toList x) ++ ")"
value y = error $ "value: unsupported argument " ++ show y
