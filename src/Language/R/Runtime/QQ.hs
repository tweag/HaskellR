-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Quasiquoting for R specialized for GHCi. This quasiquoter is functionally
-- equivalent to that defined in "Language.R.QQ", but exploits the fact that
-- Template Haskell runs in the same address space as the runtime R instance to
-- produce simpler (and marginally more efficient) expansions of quasiquotes.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.R.Runtime.QQ
  ( r
  , rexp
  ) where

import           H.Internal.Prelude
import           Language.R.HExp.Unsafe
import qualified H.Prelude as H
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R.Internal as R
import           Language.R ( parseText, withProtected, eval, install )
import           Control.Exception

-- import Control.Monad ( forM_, (>=>) )
import Data.List ( isSuffixOf )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Foreign ( ptrToIntPtr, intPtrToPtr )

-------------------------------------------------------------------------------
-- Runtime Quasi-Quoter                                                      --
-------------------------------------------------------------------------------

-- | Runtime R quasiquoter. It parses R variable, then add all required
-- substitutions, and finally evaluates the resulting expression.
r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp  = parseExpRuntimeEval
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Runtime R quasiquoter. Same as 'r', except that it doesn't evaluate the
-- expression.
rexp :: QuasiQuoter
rexp = QuasiQuoter
    { quoteExp  = parseExpRuntime
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

parseExpRuntime :: String -> Q Exp
parseExpRuntime txt = do
    ex <- runIO $ unsafeRunInRThread $ parseText txt True >>= R.protect
    {-
    - Current approach to use R memory are not correct and doesn't survive
    - gctorture(TRUE) as it has problems in convert time and compile time
    - One thing that is definitely true is that we need to protect 'ex'.
    - Another strange thing is that it seems that we need to protect ex
    - internals, however if we do it in (1) it doesn't save internals
    - from being collected. But once we use printValue (!!) there
    - everything works as expected.
    len <- runIO $ do
        len <- R.length ex
        z <-   R.expression (R.coerce ex)
        forM_ [0..len-1] $ peekElemOff z >=> R.printValue (1)
        return len
    -}
    let l = RuntimeSEXP ex
    ret <- case attachHs ex of
      [] -> [| return (unRuntimeSEXP l) |]
      x  -> [| H.withProtected (return (unRuntimeSEXP l)) (const $ $(gather x l)) |]
    runIO $ R.unprotect 1
    return ret
  where
    gather :: [ExpQ -> ExpQ] -> (RuntimeSEXP a) -> ExpQ
    gather vls l = foldr (\v t -> v t) [| return (unRuntimeSEXP l)|] vls

parseExpRuntimeEval :: String -> Q Exp
parseExpRuntimeEval txt = [| H.withProtected $(parseExpRuntime txt) eval |]

-- | Generate code to attach haskell symbols to SEXP structure.
attachHs :: SEXP a -> [ExpQ -> ExpQ]
attachHs h@(hexp -> Expr _ v) =
    concat (map (\(i, SomeSEXP t) ->
      let tl = attachHs t
          t' = RuntimeSEXP t
          s = RuntimeSEXP (R.unsafeCoerce h)
      in case haskellName t of
           Just hname ->
             [\e -> [| io (R.writeVector (unRuntimeSEXP s :: SEXP R.Expr) $(lift i) (H.mkSEXP $(varE hname))) >> $e |]]
           Nothing ->
             (\e -> [| io (R.protect (unRuntimeSEXP t')) >> $e >>= \x -> io (R.unprotect 1) >> return x|]):tl)
                $ zip [(0::Int)..] (Vector.toList v))
attachHs h@(hexp -> Lang x ls) =
  let tl = attachHs x ++ (maybe [] attachHs ls)
  in maybe tl (:tl) (attachSymbol h x)
attachHs h@(hexp -> List x tl _) =
  let tls = (attachHs x) ++ (maybe [] attachHs tl)
  in maybe tls (:tls) (attachSymbol h x)
attachHs _ = []

attachSymbol :: SEXP a -> SEXP b -> Maybe (ExpQ -> ExpQ)
attachSymbol s@(hexp -> Lang _ params) (haskellName -> Just hname) =
    let rs = RuntimeSEXP (R.sexp . R.unsexp $ s)
        rp = maybe (RuntimeSEXP (R.unsafeCoerce H.nilValue)) RuntimeSEXP params
    in Just (\e ->
         [| H.withProtected (install ".Call") $ \call ->
              H.withProtected (return $ H.mkSEXP $(varE hname)) $ \l -> do
                io $ R.setCar (unRuntimeSEXP rs) call
                io $ R.setCdr (unRuntimeSEXP rs) (unhexp (List l (Just (unRuntimeSEXP rp)) Nothing))
                $e
         |])
attachSymbol s (haskellName -> Just hname) =
    let rs = RuntimeSEXP (R.sexp . R.unsexp $ s)
    in Just (\e ->
         [| io (withProtected (evaluate $ H.mkSEXP $(varE hname)) (R.setCar (unRuntimeSEXP rs))) >> $e |])
attachSymbol _ _ = Nothing

haskellName :: SEXP a -> Maybe Name
haskellName (hexp -> Symbol (hexp -> Char (Vector.toString -> name)) _ _) =
    if "_hs" `isSuffixOf` name
    then Just . mkName $ take (length name - 3) name
    else Nothing
haskellName _ = Nothing

newtype RuntimeSEXP a = RuntimeSEXP {unRuntimeSEXP :: SEXP a}

instance Lift (RuntimeSEXP a) where
    -- XXX: it's possible that we may want to create HVal with correct
    --      ForeignPtr that will block SEXP deallocation in R
    lift (RuntimeSEXP x) = [| RuntimeSEXP (R.sexp ( intPtrToPtr (fromIntegral (ip::Integer) ))) |]
      where
        ip :: Integer
        ip = fromIntegral (ptrToIntPtr (R.unsexp x))
