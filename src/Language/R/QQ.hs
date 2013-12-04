-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.QQ
  ( r
  ) where

import qualified H.Prelude as H
import           H.HExp
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import           Language.R ( parseText )
import           Language.R.Interpreter ( runInRThread )

import Data.List ( isSuffixOf )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------
-- Compile time Quasi-Quoter                                                 --
-------------------------------------------------------------------------------

r :: QuasiQuoter
r = QuasiQuoter
      { quoteExp  = parseExpCompile
      , quotePat  = error "QuasiQuoter for patterns is not yet implemented"  -- XXX: implement
      , quoteType = error "QuasiQuoter for types is not supported"
      , quoteDec  = error "QuasiQuoter for declaration is not yet implemented"  -- XXX: implement
      }

parseExpCompile :: String -> ExpQ
parseExpCompile txt = do
     vs <- runIO $ do
       H.initialize H.defaultConfig
       ex <- runInRThread $ parseText txt
       let (Expr v) = hexp ex
       return $ map (R.sexp . R.unsexp) (Vector.toList v)
     let v = head vs
     [| v |] -- XXX: fix me allow working with lists

instance Lift (R.SEXP a) where
    -- XXX: it's possible that we may want to create HVal with correct
    --      ForeignPtr that will block SEXP deallocation in R
    lift (hexp -> Nil) = [| unhexp Nil |]
    lift (hexp -> Real vs) = [| H.mkSEXP $(return (ListE $ map (\t -> ConE (mkName "GHC.Exts.D#") `AppE` (LitE . DoublePrimL . toRational $ t)) (Vector.toList vs))) |]
    lift h@(hexp -> Lang (hexp -> Symbol (hexp -> Char (Vector.toString -> name))
                                         (hexp -> Special _)
                                         g) ls) =
      case name of
        "function" -> unsafePerformIO $ do
            error "under construction"
        _          -> [| unhexp (Lang (H.install name) $([| ls |])) |]
    lift (hexp -> Lang (hexp -> Symbol (hexp -> Char (Vector.toString -> name))
                                       (hexp -> Builtin _)
                                        g) ls) =
      -- XXX: it seems that we may assume that builtin ordering is not
      -- changed and use builtin construct instread on installing symbol
      [| unhexp (Lang (H.install name) $([| ls |])) |]
    lift (hexp -> Lang _ _) =
      error "emit/Lang: Unsupported."
    lift x@(hexp -> Symbol _n@(hexp -> Char (Vector.toString -> name)) _b c) =
      if "_hs" `isSuffixOf` name
      then
        let hname = take (length name - 3) name
        in [| H.mkSEXP $(varE (mkName hname)) |]
      else [| {-H.nameded (H.marked-} (unhexp (Symbol (H.string name) H.unboundValue Nothing)){-)-} |] --
--        if name == ""
--        then [| unhexp (Symbol (H.string "") H.unboundValue Nothing) |]
--        else [| H.install name |]
    lift (hexp -> List x mxs mtg) =
      [| unhexp (List $([| x |]) $([|mxs|]) $([|mtg|]))|]
    lift (hexp -> Char (Vector.toString -> value)) =
      -- XXX: we want somehow to reduce overhead in Ghci as this variant
      -- leads to a symbol copying
      [| H.string value |]
    lift (hexp -> String x) =
      let l = Vector.length x
      in if l == 1
         then case hexp (Vector.head x) of
                (Char v) -> let h = Vector.toString v
                            in [| H.strings h |]
                _ -> error "emit/String/1: incorrect type"
         else error "emit/String/many: not yet implemented"
    lift x = unsafePerformIO $ do
      ty <- R.typeOf x
      error $ "emit: not yet implemented ("++ show ty ++")"
