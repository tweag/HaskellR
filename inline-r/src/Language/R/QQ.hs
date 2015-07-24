-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.R.QQ
  ( r
  , rexp
  , rsafe
  ) where

import           Control.Memory.Region
import H.Internal.Prelude
import qualified H.Prelude as H
import           Language.R.HExp
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import           Language.R (parseText)

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import qualified Data.Set as Set
import Data.List (isSuffixOf)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Compile time Quasi-Quoter                                                 --
-------------------------------------------------------------------------------

-- | An R value, expressed as an R expression, in R's syntax.
r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp  = mkQQ
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Construct an R expression but don't evaluate it.
rexp :: QuasiQuoter
rexp = QuasiQuoter
    { quoteExp  = \txt -> [| io $ H.parseText txt False |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Quasiquoter for pure R code (no side effects) and that does not depend on
-- the global environment (referential transparency). This means that all
-- symbols must appear qualified with a package namespace (whose bindings are
-- locked by default), the code must not affect R shared state in any way,
-- including the global environment, and must not perform I/O.

-- TODO some of the above invariants can be checked statically. Do so.
rsafe :: QuasiQuoter
rsafe = QuasiQuoter
    { quoteExp  = \txt -> [| unsafePerformIO $ R.withProtected (H.parseText txt False)
                                             $ H.evalIO |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

mkQQ :: String -> Q TH.Exp
mkQQ input = parse input >>= \expr ->
    -- XXX: possibly we need to introduce our own parser and do not reuse R one.
    let vars = Set.toList . Set.fromList $ getNames expr
    in if null vars
          then [| do x <- io $ do expression <- H.parseText input False
                                  H.evalIO expression
                     acquireSome x
                     |]
          else [| do x <- io $ $(withVars vars [| do expression <- H.parseText input False
                                                     H.evalIO expression |])
                     acquireSome x
                     |]
  where
    withVars :: [Either String String] -> TH.ExpQ -> TH.ExpQ
    withVars []                action = action
    withVars (Left name:names) action =
      let hvar = TH.varE $ TH.mkName $ spliceNameChop name
      in [| R.withProtected (H.mkSEXPIO $hvar) $ \val ->
               H.withGloballyDefined name val $(withVars names action)
            |]
    withVars (Right name:names) action = do
      let nm = spliceNameChop name
          nmext = name ++ "_ext"
          splice = "function(...) .Call(" ++ nmext ++ ", ...)"
      hvar <- fmap (TH.varE . (maybe (TH.mkName nm) id)) (TH.lookupValueName nm)
      [| -- let expression = H.unsafeParseText splice False
         R.withProtected (H.mkSEXPIO $hvar) $ \val ->  do
           case R.typeOf val of
             R.ExtPtr ->
               H.withGloballyDefined nmext val $
                  R.withProtected (H.parseText splice False) $ \expr -> do
                     someValue <- H.evalIO expr
                     R.unSomeSEXP someValue $ \value ->
                        R.withProtected (return value) $ \protectedValue ->
                          H.withGloballyDefined name protectedValue $(withVars names action)
             _ -> H.withGloballyDefined name val $(withVars names action)
            |]
    -- | Traverse R.SEXP structure and find all occurences of the spliced
    -- variables
    getNames :: R.SEXP s a -> [Either String String]
    getNames (hexp -> Symbol pname _ _)
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = [Left name]
    getNames (hexp -> (List a b c)) = getNames a ++ getNames b ++ getNames c
    getNames (hexp -> (Lang (hexp -> Symbol pname _ _) b))
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = Right name:getNames b
    getNames (hexp -> (Lang a b)) = getNames a ++ getNames b
    getNames (hexp -> (Closure a b c)) = getNames a ++ getNames b ++ getNames c
    getNames (hexp -> (Vector _ a)) = Vector.toList a >>= (\(SomeSEXP s) -> getNames s)
    getNames (hexp -> (Expr _ a))   = Vector.toList a >>= (\(SomeSEXP s) -> getNames s)
    getNames _ = []

parse :: String -> Q (R.SEXP V 'R.Expr)
parse txt = runIO $ do
    H.initialize H.defaultConfig
    unsafeRunInRThread $ parseText txt False

-- | Returns 'True' if the variable name is in fact a Haskell value splice.
isSplice :: String -> Bool
isSplice = ("_hs" `isSuffixOf`)

-- | Chop a splice variable in order to obtain the name of the haskell variable
-- to splice.
spliceNameChop :: String -> String
spliceNameChop name = take (length name - 3) name

