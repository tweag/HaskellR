-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Language.R.Runtime.QQ
  ( r
  , rexp
  ) where

import qualified H.Prelude as H
import           H.Monad
import           H.HExp
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import Language.R ( parseText )
import Language.R.Interpreter ( runInRThread )

-- import Control.Monad ( void, unless )
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
      , quotePat  = error "rr/Pat: Unimplemented."
      , quoteType = error "rr/Type: Unimplemented."
      , quoteDec  = error "rr/Dec: Unimplemented."
      }

-- | Runtime R quasiquoter. Same as 'r', except that it doesn't evaluate the
-- expression.
rexp :: QuasiQuoter
rexp = QuasiQuoter
      { quoteExp  = parseExpRuntime
      , quotePat  = error "rr/Pat: Unimplemented."
      , quoteType = error "rr/Type: Unimplemented."
      , quoteDec  = error "rr/Dec: Unimplemented."
      }

parseExpRuntime :: String -> Q Exp
parseExpRuntime txt = do
    ex <- runIO $ runInRThread $ parseText txt True
    let l = RuntimeSEXP ex
    case attachHs ex of
      [] -> [| return (unRuntimeSEXP l) |]
      x  -> [| $(gather x l) |]
  where
    gather :: [ExpQ -> ExpQ] -> (RuntimeSEXP a) -> ExpQ
    gather vls l = foldr (\v t -> v t) [| return (unRuntimeSEXP l)|] vls

parseExpRuntimeEval :: String -> Q Exp
parseExpRuntimeEval txt = [| H.eval =<< $(parseExpRuntime txt) |]

-- | Generate code to attach haskell symbols to SEXP structure.
attachHs :: R.SEXP a -> [ExpQ -> ExpQ]
attachHs h@(hexp -> Expr _ v) =
    concat (map (\(i,t) ->
      let tl = attachHs t
      in case haskellName t of
           Just hname ->
             [\e -> [| io (R.setExprElem (unRuntimeSEXP s) i (H.mkSEXP $(varE hname))) >> $e |]]
           Nothing -> tl)
                $ zip [(0::Int)..] (Vector.toList v))
  where
    s = RuntimeSEXP (R.sexp . R.unsexp $ h)
attachHs h@(hexp -> Lang x ls) =
  let tl = attachHs x ++ attachHs ls
  in maybe tl (:tl) (attachSymbol h x)
attachHs h@(hexp -> List x tl _) =
  let tls = (attachHs x) ++ (maybe [] attachHs tl)
  in maybe tls (:tls) (attachSymbol h x)
attachHs _ = []

attachSymbol :: R.SEXP a -> R.SEXP b -> Maybe (ExpQ -> ExpQ)
attachSymbol s@(hexp -> Lang _ params) (haskellName -> Just hname) =
    let rs = RuntimeSEXP (R.sexp . R.unsexp $ s)
        rp = RuntimeSEXP params
    in Just (\e ->
         [| H.withProtected (H.install ".Call") $ \call ->
              H.withProtected (return $ H.mkSEXP $(varE hname)) $ \l -> do
                io $ R.setCar (unRuntimeSEXP rs) call
                io $ R.setCdr (unRuntimeSEXP rs) (unhexp (List l (Just (unRuntimeSEXP rp)) Nothing))
                $e
         |])
attachSymbol s (haskellName -> Just hname) =
    let rs = RuntimeSEXP (R.sexp . R.unsexp $ s)
    in Just (\e ->
         [| H.withProtected (return $ H.mkSEXP $(varE hname)) $ \l -> io $ R.setCar (unRuntimeSEXP rs) l >> $e |])
attachSymbol _ _ = Nothing

haskellName :: R.SEXP a -> Maybe Name
haskellName (hexp -> Symbol (hexp -> Char (Vector.toString -> name)) _ _) =
    if "_hs" `isSuffixOf` name
    then Just . mkName $ take (length name - 3) name
    else Nothing
haskellName _ = Nothing

newtype RuntimeSEXP a = RuntimeSEXP {unRuntimeSEXP :: R.SEXP a}

instance Lift (RuntimeSEXP a) where
    -- XXX: it's possible that we may want to create HVal with correct
    --      ForeignPtr that will block SEXP deallocation in R
    lift (RuntimeSEXP x) = [| RuntimeSEXP (R.sexp ( intPtrToPtr (fromIntegral (ip::Integer) ))) |]
      where
        ip :: Integer
        ip = fromIntegral (ptrToIntPtr (R.unsexp x))
