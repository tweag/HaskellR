-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
-- For the 'Vector' instance of 'Lift'.
{-# LANGUAGE OverlappingInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.R.QQ
  ( r
  , rexp
  , rsafe
  ) where

import H.Internal.Prelude
import qualified H.Prelude as H
import           Language.R.HExp
import           Language.R.Literal
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import qualified Foreign.R.Type as SingR
import           Language.R (parseText, installIO, string, eval)

import qualified Data.ByteString.Char8 as BS

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Lift (deriveLift)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Control.Monad ((>=>))
import Data.List (isSuffixOf)
import Data.Complex (Complex)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Compile time Quasi-Quoter                                                 --
-------------------------------------------------------------------------------

-- | An R value, expressed as an R expression, in R's syntax.
r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp  = \txt -> parseEval txt
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Construct an R expression but don't evaluate it.
rexp :: QuasiQuoter
rexp = QuasiQuoter
    { quoteExp  = \txt -> [| return $(parseExp txt) |]
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
    { quoteExp  = \txt -> [| unsafePerformIO $ unsafeRToIO $ eval $(parseExp txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

parseEval :: String -> Q TH.Exp
parseEval txt = do
    sexp <- parse txt
    case hexp sexp of
      Expr _ v ->
        let vs = Vector.toList v
        in go vs
      _ -> error "Impossible happen."
  where
    go :: [SomeSEXP] -> Q TH.Exp
    go []     = error "Impossible happen."
    go [SomeSEXP a]    = [| H.withProtected (return a) eval |]
    go (SomeSEXP a:as) =
        [| H.withProtected (return a) $ eval >=> \(SomeSEXP s) ->
             H.withProtected (return s) (const $(go as))
         |]

parse :: String -> Q (R.SEXP R.Expr)
parse txt = runIO $ do
    H.initialize H.defaultConfig
    parseText txt False

parseExp :: String -> Q TH.Exp
parseExp txt = TH.lift =<< parse txt

-- XXX Orphan instance defined here due to bad interaction betwen TH and c2hs.
deriveLift ''SEXPInfo
deriveLift ''SomeSEXP
deriveLift ''Complex
deriveLift ''R.Logical
deriveLift ''HExp

instance TH.Lift BS.ByteString where
    lift bs = let s = BS.unpack bs in [| BS.pack s |]

instance TH.Lift Int32 where
    lift x = let x' = fromIntegral x :: Integer in [| fromInteger x' :: Int32 |]

instance TH.Lift Word8 where
    lift x = let x' = fromIntegral x :: Integer in [| fromInteger x' :: Word8 |]

instance TH.Lift Double where
    lift x = [| $(return $ TH.LitE $ TH.RationalL $ toRational x) :: Double |]

instance TH.Lift (Vector.Vector R.Raw Word8) where
    -- Apparently R considers 'allocVector' to be "defunct" for the CHARSXP
    -- type. So we have to use some bespoke function.
    lift v = let xs :: String
                 xs = map (toEnum . fromIntegral) $ Vector.toList v
             in [| vector $ unsafePerformIO $ string xs |]

instance TH.Lift (Vector.Vector R.Char Word8) where
    -- Apparently R considers 'allocVector' to be "defunct" for the CHARSXP
    -- type. So we have to use some bespoke function.
    lift v = let xs :: String
                 xs = map (toEnum . fromIntegral) $ Vector.toList v
             in [| vector $ unsafePerformIO $ string xs |]

instance TH.Lift (Vector.Vector 'R.Logical R.Logical) where
    lift v = let xs = Vector.toList v
             in [| vector (mkSEXPVector SingR.SLogical xs) |]

instance TH.Lift (Vector.Vector R.Int Int32) where
    lift v = let xs = Vector.toList v
             in [| vector (mkSEXPVector SingR.SInt xs) |]

instance TH.Lift (Vector.Vector R.Real Double) where
    lift v = let xs = Vector.toList v
             in [| vector (mkSEXPVector SingR.SReal xs) |]

instance TH.Lift (Vector.Vector R.Complex (Complex Double)) where
    lift v = let xs = Vector.toList v
             in [| vector (mkSEXPVector SingR.SComplex xs) |]

instance TH.Lift (Vector.Vector R.String (SEXP R.Char)) where
    lift v = let xs = Vector.toList v
             in [| vector $ mkProtectedSEXPVector SingR.SString xs |]

instance TH.Lift (Vector.Vector R.Vector SomeSEXP) where
    lift v = let xs = map (\(SomeSEXP s) -> castPtr s) $ Vector.toList v :: [SEXP R.Any]
             in [| vector $ mkProtectedSEXPVector SingR.SVector xs |]

instance TH.Lift (Vector.Vector R.Expr SomeSEXP) where
    lift v = let xs = map (\(SomeSEXP s) -> castPtr s) $ Vector.toList v :: [SEXP R.Any]
             in [| vector $ mkProtectedSEXPVector SingR.SExpr xs |]

-- Bogus 'Lift' instance for pointers because 'deriveLift' blindly tries to cope
-- with 'H.ExtPtr' when this is in fact not possible.
instance TH.Lift (Ptr ()) where
    lift _ = violation "TH.Lift.lift Ptr" "Attempted to lift a pointer."

-- | Returns 'True' if the variable name is in fact a Haskell value splice.
isSplice :: String -> Bool
isSplice = ("_hs" `isSuffixOf`)

-- | Chop a splice variable in order to obtain the name of the haskell variable
-- to splice.
spliceNameChop :: String -> String
spliceNameChop name = take (length name - 3) name

instance TH.Lift (SEXP a) where
    -- Special case some forms, rather than relying on the default code
    -- generated by 'deriveLift'.
    lift   (hexp -> Symbol pname _ (Just _)) = do
        [| unsafePerformIO (installIO xs) |]
      where
        xs :: String
        xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
    lift (hexp -> List s Nothing Nothing)
      | R.unsexp s == R.unsexp H.missingArg =
        [| unsafePerformIO $ R.cons H.missingArg H.nilValue |]
    lift s@(hexp -> Symbol pname value _)
      | R.unsexp s == R.unsexp value = do
        [| unsafePerformIO $ selfSymbol pname |]    -- FIXME
    lift   (hexp -> Symbol pname _ Nothing)
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = do
        let hvar = TH.varE $ TH.mkName $ spliceNameChop name
        [| H.mkSEXP $hvar |]
      | otherwise =
        [| unsafePerformIO $ installIO xs |]        -- FIXME
      where
        xs :: String
        xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
    lift (hexp -> Lang (hexp -> Symbol pname _ Nothing) rands)
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = do
        let nm = spliceNameChop name
        hvar <- fmap (TH.varE . (maybe (TH.mkName nm) id)) (TH.lookupValueName nm)
        [| let call = unsafePerformIO (installIO ".Call")
               f    = H.mkSEXP $hvar
             in unhexp $ Lang call (Just (unhexp $ List f rands Nothing)) |]
    -- Override the default for expressions because the default Lift instance
    -- for vectors will allocate a node of VECSXP type, when the node is real an
    -- EXPRSXP.
    lift   (hexp -> Expr n v) =
      let xs = Vector.toList v
      in [| unhexp $ Expr n $ vector $ mkSEXPVector SingR.SExpr xs |]
    lift   (hexp -> t) =
        [| unhexp t |]
