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

module Language.R.QQ
  ( r
  , rexp
  , rsafe
  ) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import           Foreign.R (SEXP, SomeSEXP(..))
import qualified H.Prelude as H
import           Internal.Error
import           Language.R (parseText, eval)
import           Language.R.HExp
import           Language.R.Instance
import           Language.R.Literal (mkSEXPIO)

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Control.Concurrent (MVar, newMVar, withMVar)
import Data.List (intercalate, isSuffixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Compile time Quasi-Quoter                                                 --
-------------------------------------------------------------------------------

-- | An R value, expressed as an R expression, in R's syntax.
r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp = \txt -> [| eval =<< $(expQQ txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Construct an R expression but don't evaluate it.
rexp :: QuasiQuoter
rexp = QuasiQuoter
    { quoteExp  = unimplemented "quoteExp"
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
    { quoteExp  = \txt -> [| unsafePerformIO $ unsafeRToIO . eval =<< $(expQQ txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Serialize quasiquotes using a global lock, because the compiler is allowed
-- in theory to run them in parallel, yet the R runtime is not reentrant.
qqLock :: MVar ()
qqLock = unsafePerformIO $ newMVar ()
{-# NOINLINE qqLock #-}

parse :: String -> Q (R.SEXP V 'R.Expr)
parse txt = runIO $ do
    H.initialize H.defaultConfig
    withMVar qqLock $ \_ -> parseText txt False

isAnti :: SEXP s 'R.Char -> Bool
isAnti (hexp -> Char (Vector.toString -> name)) = "_hs" `isSuffixOf` name
isAnti _ = error "Impossible"

-- | Chop antiquotation variable names to get the corresponding Haskell variable name.
chop :: String -> String
chop name = take (length name - 3) name

-- | Traverse 'R.SEXP' structure and find all occurences of antiquotations.
collectAntis :: R.SEXP s a -> Set (SEXP s 'R.Char)
collectAntis (hexp -> Symbol name _ _)
  | isAnti name = Set.singleton name
collectAntis (hexp -> (List sxa sxb sxc)) = do
    Set.unions [collectAntis sxa, collectAntis sxb, collectAntis sxc]
collectAntis (hexp -> (Lang (hexp -> Symbol name _ _) sxb))
  | isAnti name = Set.insert name (collectAntis sxb)
collectAntis (hexp -> (Lang sxa sxb)) =
    Set.union (collectAntis sxa) (collectAntis sxb)
collectAntis (hexp -> (Closure sxa sxb sxc)) =
    Set.unions [collectAntis sxa, collectAntis sxb, collectAntis sxc]
collectAntis (hexp -> (Vector _ sxv)) =
    Set.unions [collectAntis sx | SomeSEXP sx <- Vector.toList sxv]
collectAntis (hexp -> (Expr _ sxv)) =
    Set.unions [collectAntis sx | SomeSEXP sx <- Vector.toList sxv]
collectAntis _ = Set.empty

-- | An R quasiquote is syntactic sugar for a function that we
-- generate, which closes over all antiquotation variables, and applies the
-- function to the Haskell values to which those variables are bound. Example:
--
-- @
-- [r| x_hs + y_hs |] ==> apply (apply [r| function(x_hs, y_hs) x_hs + y_hs |] x) y
-- @
expQQ :: String -> Q TH.Exp
expQQ input = do
    expr <- parse input
    let antis = [x | (hexp -> Char (Vector.toString -> x))
                       <- Set.toList (collectAntis expr)]
        args = map (TH.dyn . chop) antis
        closure = "function(" ++ intercalate "," antis ++ "){" ++ input ++ "}"
        z = [| return (R.release H.nilValue) |]
    vars <- mapM (\_ -> TH.newName "x") antis
    -- Abstract over antis using fresh vars, to avoid captures with names bound
    -- internally (such as 'f' below).
    (\body -> foldl TH.appE body args) $ TH.lamE (map TH.varP vars)
      [| do -- Memoize the runtime parsing of the generated closure (provided the
            -- compiler notices that it can let-float to top-level).
            let sx = unsafePerformIO $ do
                       exprs <- parseText closure False
                       SomeSEXP e <- R.indexVector exprs 0
                       clos <- R.eval e (R.release H.globalEnv)
                       R.unSomeSEXP clos R.preserveObject
                       return clos
            io $ case sx of
              SomeSEXP f ->
                R.lcons f =<<
                  $(foldr (\x xs -> [| R.withProtected $xs $ \cdr -> do
                                         car <- mkSEXPIO $(TH.varE x)
                                         R.lcons car cdr |]) z vars)
       |]
