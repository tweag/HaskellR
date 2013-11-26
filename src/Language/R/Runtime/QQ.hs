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
import           H.HExp
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import qualified Foreign.R.Parse as R
import Language.R ( nilValue, withProtected )

import Data.List ( isSuffixOf )
import Data.IORef ( readIORef )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Foreign ( alloca
               , ptrToIntPtr, intPtrToPtr )
import Foreign.C.String ( withCString )
import System.IO.Unsafe ( unsafePerformIO )

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
     ex <- runIO $ withCString txt $ \ctxt -> do
             rtxt <- R.mkString ctxt
             -- XXX: this is a hack due to incorrect address mapping in ghci
             --      it requires Language.R.nilValue to be set before running
             nil <- readIORef nilValue
             alloca $ \status ->
               R.parseVector rtxt (-1) status nil

     let l = RuntimeSEXP ex
     case attachHs ex of
         [] -> [| unRuntimeSEXP l |]
         x  -> [| unsafePerformIO $(gather x l) |]
  where
    gather :: [ExpQ -> ExpQ] -> (RuntimeSEXP a) -> ExpQ
    gather vls l = foldr (\v t -> v t) [| return (unRuntimeSEXP l)|] vls

parseExpRuntimeEval :: String -> Q Exp
parseExpRuntimeEval txt = [| H.eval $(parseExpRuntime txt) |]

-- | Generate code to attach haskell symbols to SEXP structure.
attachHs :: R.SEXP a -> [ExpQ -> ExpQ]
attachHs (hexp -> Expr v) =
  concat (map attachHs (Vector.toList v))
attachHs (hexp -> Lang x@(hexp -> Lang{}) ls) =
  attachHs x ++ attachHs ls
attachHs (hexp -> Lang x@(hexp -> List{}) ls) =
  attachHs x ++ attachHs ls
attachHs (hexp -> Lang _ ls) = attachHs ls
attachHs (hexp -> List x@(hexp -> Lang{}) tl _) =
  attachHs x ++ (maybe [] attachHs tl)
attachHs (hexp -> List x@(hexp -> List{}) tl _) =
  attachHs x ++ (maybe [] attachHs tl)
attachHs h@(hexp -> List x@(hexp -> Symbol{}) tl _) =
  case attachList h x of
      Just z -> z:maybe [] attachHs tl
      Nothing -> maybe [] attachHs tl
attachHs _ = []

attachList :: R.SEXP R.List -> R.SEXP b -> Maybe (ExpQ -> ExpQ)
attachList s@(hexp -> List _ tl tg) (hexp -> Symbol (hexp -> Char (Vector.toString -> name)) _ _) =
    if "_hs" `isSuffixOf` name
    then
      let hname = take (length name - 3) name
          rs = RuntimeSEXP s
      in Just (\e ->
                    [| withProtected (return $ H.mkSEXP $(varE (mkName hname))) $ \l -> injectList (unRuntimeSEXP rs) l >> $e |])
    else Nothing
attachList _ _ = Nothing

newtype RuntimeSEXP a = RuntimeSEXP {unRuntimeSEXP :: R.SEXP a}

instance Lift (RuntimeSEXP a) where
    -- XXX: it's possible that we may want to create HVal with correct
    --      ForeignPtr that will block SEXP deallocation in R
    lift (RuntimeSEXP x) = [| RuntimeSEXP (R.sexp ( intPtrToPtr (fromIntegral (ip::Integer) ))) |]
      where
        ip :: Integer
        ip = fromIntegral (ptrToIntPtr (R.unsexp x))
