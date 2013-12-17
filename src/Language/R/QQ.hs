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
import H.Internal.REnv
import qualified H.Prelude as H
import           H.HExp
import           H.Internal.Literal
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import           Language.R (parseText, install, string)
import           Language.R.Interpreter (runInRThread)

import qualified Data.ByteString.Char8 as BS

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Lift (deriveLift)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Data.List (isSuffixOf)
import Data.Complex (Complex)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Compile time Quasi-Quoter                                                 --
-------------------------------------------------------------------------------

r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp  = \txt -> [| H.eval $(parseExp txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

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
    { quoteExp  = \txt -> [| unsafePerformIO $ runR REnv $ H.eval $(parseExp txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

parseExp :: String -> Q TH.Exp
parseExp txt = do
    sexp <- runIO $ do
       _ <- H.initialize H.defaultConfig
       runInRThread $ parseText txt False
    TH.lift sexp

-- XXX Orphan instance defined here due to bad interaction betwen TH and c2hs.
deriveLift ''SEXPInfo
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

instance TH.Lift (Vector.Vector Word8) where
    -- Apparently R considers 'allocVector' to be "defunct" for the CHARSXP
    -- type. So we have to use some bespoke function.
    lift v = let xs :: String
                 xs = map (toEnum . fromIntegral) $ Vector.toList v
             in [| vector $ unsafePerformIO $ string xs |]

instance TH.Lift (Vector.Vector R.Logical) where
    lift v = let xs = Vector.toList v in [| vector $ mkSEXPVector R.Logical xs |]

instance TH.Lift (Vector.Vector Int32) where
    lift v = let xs = Vector.toList v in [| vector $ mkSEXPVector R.Int xs |]

instance TH.Lift (Vector.Vector Double) where
    lift v = let xs = Vector.toList v in [| vector $ mkSEXPVector R.Real xs |]

instance TH.Lift (Vector.Vector (Complex Double)) where
    lift v = let xs = Vector.toList v in [| vector $ mkSEXPVector R.Complex xs |]

-- TODO Special case for R.Expr.
instance TH.Lift (Vector.Vector (SEXP (R.Vector Word8))) where
    lift v = let xs = Vector.toList v in [| vector $ mkSEXPVector R.String xs |]

instance TH.Lift (Vector.Vector (SEXP R.Any)) where
    lift v = let xs = TH.listE (map (\t -> [| R.coerce t|]) $ Vector.toList v) in
      [| vector $ mkSEXPVector (R.Vector R.Any) $xs |]

instance TH.Lift (Vector.Vector (SEXP a)) where
    lift v = let xs = Vector.toList v in
      [| vector $ mkSEXPVector (R.Vector R.Any) xs |]

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
        [| unsafePerformIO (install xs) |]
      where
        xs :: String
        xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
    lift s@(hexp -> Symbol pname value _)
      | R.unsexp s == R.unsexp value = do
        [| unsafePerformIO $ selfSymbol pname |]    -- FIXME
    lift   (hexp -> Symbol pname _ Nothing)
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = do
        let hvar = TH.varE $ TH.mkName $ spliceNameChop name
        [| H.mkSEXP $hvar |]
      | otherwise =
        [| unsafePerformIO $ install xs |]        -- FIXME
      where
        xs :: String
        xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
    lift s@(hexp -> Lang (hexp -> Symbol pname _ Nothing) rands)
      | Char (Vector.toString -> name) <- hexp pname
      , isSplice name = do
        let hvar = TH.varE $ TH.mkName $ spliceNameChop name
        [| unsafePerformIO $
             let call = unsafePerformIO (install ".Call")
                 f    = H.mkSEXP $hvar
             in return $ unhexp $ Lang call (Just (unhexp $ List f rands Nothing)) |]
    lift   (hexp -> t) =
        [| unhexp t |] 
