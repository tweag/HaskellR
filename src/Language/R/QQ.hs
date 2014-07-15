-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE LambdaCase #-}
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

import           H.Internal.Error
import           H.Prelude (io)
import qualified H.Prelude as H
import           Control.Monad.R.Unsafe (unsafeIOToR, unsafePerformR)
import           Language.R.HExp.Unsafe
import           Language.R.Literal.Unsafe
import           Language.R.Globals.Unsafe
import qualified Data.Vector.SEXP as Vector
import           Foreign.R.Internal (SEXP, SomeSEXP(..), SEXPInfo(..))
import qualified Foreign.R.Internal as R
import qualified Foreign.R.Type as SingR
import           Language.R (parseText, installIO, string, eval, evalIO)
import           Foreign.R.Internal (withProtected)

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
import Foreign (castPtr)

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
    { quoteExp  = \txt -> [| unsafeIOToR $(parseExp txt) |]
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
    { quoteExp  = \txt -> [| unsafePerformR  $ eval =<< io $(parseExp txt) |]
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
        in case vs of
	    [] -> error "Impossible happen."
            [SomeSEXP (returnIO -> a)] -> 
	       [| H.protect =<< Control.Monad.R.Unsafe.unsafeIOToR (withProtected a evalIO) |]
	    xs -> [| H.protect =<< Control.Monad.R.Unsafe.unsafeIOToR $(go xs) |]
      _ -> error "Impossible happen."
  where
    go :: [SomeSEXP] -> Q TH.Exp
    go []     = error "Impossible happen."
    go [(SomeSEXP (returnIO -> a))] = [| withProtected a evalIO |]
    go (SomeSEXP (returnIO -> a) : as) =
        [| withProtected a $ evalIO >=> \(SomeSEXP s) ->
             R.withProtected (return s) (const $(go as))
         |]

returnIO :: a -> IO a
returnIO = return

parse :: String -> Q (R.SEXP R.Expr)
parse txt = runIO $ do
    H.initialize H.defaultConfig
    parseText txt False

parseExp :: String -> Q TH.Exp
parseExp txt = TH.lift . returnIO =<< parse txt

-- XXX Orphan instance defined here due to bad interaction betwen TH and c2hs.
instance TH.Lift (IO SomeSEXP) where
  lift = runIO >=> \s -> R.unSomeSEXP s (TH.lift . returnIO)

deriveLift ''SEXPInfo
deriveLift ''Complex
deriveLift ''R.Logical

instance TH.Lift (IO (Maybe (SEXP a))) where
  lift = runIO >=> return . fmap returnIO >=>
           maybe [| return Nothing |] (\vio -> [| fmap Just vio |])

instance TH.Lift (IO [SEXP a]) where
    lift = runIO >=> go
      where
        go []                       = [| return [] |]
        go [returnIO -> xio]        = [| xio >>= return . (:[]) |]
        go ((returnIO -> xio) : xs) =
          [| withProtected xio $ $(go xs) . fmap . (:) |]

instance TH.Lift BS.ByteString where
    lift bs = let s = BS.unpack bs in [| BS.pack s |]

instance TH.Lift Int32 where
    lift x = let x' = fromIntegral x :: Integer in [| fromInteger x' :: Int32 |]

instance TH.Lift Word8 where
    lift x = let x' = fromIntegral x :: Integer in [| fromInteger x' :: Word8 |]

instance TH.Lift Double where
    lift x = [| $(return $ TH.LitE $ TH.RationalL $ toRational x) :: Double |]

instance TH.Lift (IO (Vector.Vector R.Raw Word8)) where
    -- Apparently R considers 'allocVector' to be "defunct" for the CHARSXP
    -- type. So we have to use some bespoke function.
    lift = runIO >=> \v -> do
      let xs :: String
          xs = map (toEnum . fromIntegral) $ Vector.toList v
      [| fmap vector $ string xs |]

instance TH.Lift (IO (Vector.Vector R.Char Word8)) where
    -- Apparently R considers 'allocVector' to be "defunct" for the CHARSXP
    -- type. So we have to use some bespoke function.
    lift = runIO >=> \ v -> do
      let xs :: String
          xs = map (toEnum . fromIntegral) $ Vector.toList v
      [| fmap vector $ string xs |]

instance TH.Lift (IO (Vector.Vector 'R.Logical R.Logical)) where
    lift = runIO >=> \v -> do
      let xs = Vector.toList v
      [| fmap vector $ mkSEXPVectorIO SingR.SLogical xs |]

instance TH.Lift (IO (Vector.Vector R.Int Int32)) where
    lift = runIO >=> \v -> do
      let xs = Vector.toList v
      [| fmap vector $ mkSEXPVectorIO SingR.SInt xs |]

instance TH.Lift (IO (Vector.Vector R.Real Double)) where
    lift = runIO >=> \v -> do
      let xs = Vector.toList v
      [| fmap vector $ mkSEXPVectorIO SingR.SReal xs |]

instance TH.Lift (IO (Vector.Vector R.Complex (Complex Double))) where
    lift = runIO >=> \v -> do
      let xs = Vector.toList v
      [| fmap vector $ mkSEXPVectorIO SingR.SComplex xs |]

instance TH.Lift (IO (Vector.Vector R.String (SEXP R.Char))) where
    lift = runIO >=> \v -> do
      let xsio = returnIO $ Vector.toList v
      [| fmap vector . mkProtectedSEXPVectorIO SingR.SString =<< xsio |]

instance TH.Lift (IO (Vector.Vector R.Vector SomeSEXP)) where
    lift = runIO >=> \v -> do
      let xsio = returnIO $ map (\(SomeSEXP s) -> castPtr s)
                          $ Vector.toList v :: IO [SEXP R.Any]
      [| fmap vector $ mkProtectedSEXPVectorIO SingR.SVector =<< xsio |]

instance TH.Lift (IO (Vector.Vector R.Expr SomeSEXP)) where
    lift = runIO >=> \v -> do
      let xsio = returnIO $ map (\(SomeSEXP s) -> castPtr s)
                          $ Vector.toList v :: IO [SEXP R.Any]
      [| fmap vector . mkProtectedSEXPVectorIO SingR.SExpr =<< xsio |]

-- | Returns 'True' if the variable name is in fact a Haskell value splice.
isSplice :: String -> Bool
isSplice = ("_hs" `isSuffixOf`)

-- | Chop a splice variable in order to obtain the name of the haskell variable
-- to splice.
spliceNameChop :: String -> String
spliceNameChop name = take (length name - 3) name

instance TH.Lift (IO (SEXP a)) where
    -- Special case some forms, rather than relying on the default code
    -- generated by 'deriveLift'.
    lift = runIO >=> \case
      (hexp -> Symbol pname _ (Just _)) -> [| installIO xs |]
        where
          xs :: String
          xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
      (hexp -> List s Nothing Nothing)
        | R.unsexp s == R.unsexp missingArg ->
          [| R.cons missingArg nilValue |]
      s@(hexp -> Symbol (returnIO -> pnameio) value _)
        | R.unsexp s == R.unsexp value -> [| selfSymbol =<< pnameio |] -- FIXME
      (hexp -> Symbol pname _ Nothing)
        | Char (Vector.toString -> name) <- hexp pname
        , isSplice name -> do
          let hvar = TH.varE $ TH.mkName $ spliceNameChop name
          [| H.unsafeMkSEXP $hvar |]
        | otherwise -> [| installIO xs |]        -- FIXME
       where
        xs :: String
        xs = map (toEnum . fromIntegral) $ Vector.toList $ vector pname
      (hexp -> Lang (hexp -> Symbol pname _ Nothing) (returnIO -> randsio))
        | Char (Vector.toString -> name) <- hexp pname
        , isSplice name -> do
          let nm = spliceNameChop name
          hvar <- fmap (TH.varE . (maybe (TH.mkName nm) id)) (TH.lookupValueName nm)
          [| withProtected (installIO ".Call") $ \call ->
             withProtected (H.unsafeMkSEXP $hvar) $ \f -> do
                rands <- randsio
                unsafeUnhexp . Lang call . Just =<< unsafeUnhexp (List f rands Nothing)
           |]
    -- Override the default for expressions because the default Lift instance
    -- for vectors will allocate a node of VECSXP type, when the node is real an
    -- EXPRSXP.
      (hexp -> Expr n v) ->
        let xsio = returnIO $ map (\(SomeSEXP s) -> castPtr s)
                            $ Vector.toList v :: IO [SEXP R.Any]
         in [| withProtected (mkProtectedSEXPVectorIO SingR.SExpr =<< xsio) $
                 unsafeUnhexp . Expr n . vector
             |]
      (returnIO . hexp -> iot) ->
        [| unsafeUnhexp =<< iot |]

instance TH.Lift (IO (HExp a)) where
  lift = runIO >=> \case
    Nil -> [| return Nil |]
    Symbol (returnIO -> x0io) (returnIO -> x1io) (returnIO -> x2io) ->
      [| withProtected x0io $ \x0 ->
         withProtected x1io $ \x1 ->
           fmap (Symbol x0 x1) x2io
        |]
    List (returnIO -> x0io) x1m (returnIO -> x2io) ->
      [| withProtected x0io $ \x0 ->
           $(case x1m of
               Nothing  -> [| fmap (List x0 Nothing) x2io |]
               Just (returnIO -> x1io) ->
                 [| withProtected x1io $ \x1 ->
                      fmap (List x0 (Just x1)) x2io
                  |]
            )
        |]
    Env (returnIO -> x0io) (returnIO -> x1io) (returnIO -> x2io) ->
      [| withProtected x0io $ \x0 ->
         withProtected x1io $ \x1 ->
           fmap (Env x0 x1) x2io
        |]
    Closure (returnIO -> x0io) (returnIO -> x1io) (returnIO -> x2io) ->
      [| withProtected x0io $ \x0 ->
         withProtected x1io $ \x1 ->
           fmap (Closure x0 x1) x2io
        |]
    Promise (returnIO -> x0io) (returnIO -> x1io) (returnIO -> x2io) ->
      [| withProtected x0io $ \x0 ->
         withProtected x1io $ \x1 ->
           fmap (Promise x0 x1) x2io
        |]
    Lang (returnIO -> x0io) (returnIO -> x1io) ->
      [| withProtected x0io $ \x0 ->
           fmap (Lang x0) x1io
        |]
    Special                  x0  -> [| return $ Special x0 |]
    Builtin                  x0  -> [| return $ Builtin x0 |]
    Char      (returnIO -> x0io) -> [| fmap Char      x0io |]
    Logical   (returnIO -> x0io) -> [| fmap Logical   x0io |]
    Int       (returnIO -> x0io) -> [| fmap Int       x0io |]
    Real      (returnIO -> x0io) -> [| fmap Real      x0io |]
    Complex   (returnIO -> x0io) -> [| fmap Complex   x0io |]
    String    (returnIO -> x0io) -> [| fmap String    x0io |]
    DotDotDot (returnIO -> x0io) -> [| fmap DotDotDot x0io |]
    Vector x0 (returnIO -> x1io) -> [| fmap (Vector x0) x1io |]
    Expr   x0 (returnIO -> x1io) -> [| fmap (Expr x0) x1io |]
    Bytecode -> [| return Bytecode |]
    ExtPtr _ _ _ -> violation "TH.Lift.lift HExp" "Attempted to lift an ExtPtr."
    WeakRef (returnIO -> x0io) (returnIO -> x1io)
            (returnIO -> x2io) (returnIO -> x3io) ->
      [| withProtected x0io $ \x0 ->
         withProtected x1io $ \x1 ->
         withProtected x2io $ \x2 ->
           fmap (WeakRef x0 x1 x2) x3io
        |]
    Raw (returnIO -> x0io) -> [| fmap Raw x0io |]
    S4  (returnIO -> x0io) -> [| fmap S4  x0io |]
