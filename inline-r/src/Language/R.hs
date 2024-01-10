-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low-level R functions.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# Language GADTs #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}

{-@ LIQUID "--exact-data-cons" @-} -- needed to have LH accept specs in module HExp
{-@ LIQUID "--prune-unsorted" @-}
module Language.R
  ( module Foreign.R
  , module Foreign.R.Type
  , module Language.R.Instance
  , module Language.R.Globals
  , module Language.R.GC
  , module Language.R.Literal
  -- * Evaluation
  , eval
  , eval_
  , evalEnv
  , install
  , cancel
  -- * Exceptions
  , throwR
  , throwRMessage
  -- * Deprecated
  , parseFile
  , parseText
  , string
  , strings
  ) where

import           Control.Memory.Region
import qualified Data.Vector.SEXP as Vector
import Control.Monad.R.Class
import Foreign.C.String -- XXX: Needed for LH name resolution
import Foreign.ForeignPtr -- XXX: imported to help LH name resolution
import Foreign.R
  ( SEXP
  , typeOf
  )
import qualified Foreign.R as R
import qualified Foreign.R.Internal as R
import qualified Foreign.R.Parse as R
import qualified Foreign.R.Error as R
import           Foreign.R.Type
import           GHC.ST -- Needed to help LH name resolution
import           Language.R.GC
import           Language.R.Globals
import           Language.R.HExp
import           Language.R.Instance
import           {-# SOURCE #-} Language.R.Internal
import           Language.R.Literal

import Control.Applicative
import Control.Exception ( throwIO )
import Control.Monad ( (>=>), when, unless, forM, void )
import Data.ByteString as B
import Data.ByteString.Char8 as C8 ( pack, unpack )
import Foreign
  ( alloca
  , castPtr
  , peek
  , poke
  )
import Foreign.C.String ( withCString, peekCString )
import Prelude

-- NOTE: In this module, cannot use quasiquotations, since we are lower down in
-- the dependency hierarchy.

-- | Parse and then evaluate expression.
{-@ ignore parseEval @-}
parseEval :: ByteString -> IO (SEXP V)
parseEval txt = useAsCString txt $ \ctxt ->
  R.withProtected (R.mkString ctxt) $ \rtxt ->
    alloca $ \status -> do
      R.withProtected (R.parseVector rtxt 1 status (R.release nilValue)) $ \exprs -> do
        rc <- fromIntegral <$> peek status
        unless (R.PARSE_OK == toEnum rc) $
          runRegion $ throwRMessage $ "Parse error in: " ++ C8.unpack txt
        expr <- peek $ castPtr $ R.unsafeSEXPToVectorPtr exprs
        runRegion $ do
          val <- eval expr
          return (R.release val)

-- | Parse file and perform some actions on parsed file.
--
-- This function uses continuation because this is an easy way to make
-- operations GC-safe.
{-@ assume parseFile :: FilePath -> (TSEXP s Foreign.R.Type.Expr -> IO a) -> IO a @-}
parseFile :: FilePath -> (SEXP s -> IO a) -> IO a
{-# DEPRECATED parseFile "Use [r| parse(file=\"path/to/file\") |] instead." #-}
parseFile fl f = do
    withCString fl $ \cfl ->
      R.withProtected (R.mkString cfl) $ \rfl ->
        r1 (C8.pack "parse") rfl >>= \s ->
          return s `R.withProtected` f

{-@ parseText :: String -> Bool -> IO (TSEXP V Foreign.R.Type.Expr) @-}
parseText
  :: String -- ^ Text to parse
  -> Bool   -- ^ Whether to annotate the AST with source locations.
  -> IO (R.SEXP V)
{-# DEPRECATED parseText "Use [r| parse(text=...) |] instead." #-}
parseText txt b = do
    s <- parseEval $ C8.pack $
         "parse(text=" ++ show txt ++ ", keep.source=" ++ keep ++ ")"
    return $ R.Expr `R.checkSEXPTYPE` s
  where
    keep | b         = "TRUE"
         | otherwise = "FALSE"

-- | Internalize a symbol name.
{-@ assume install :: String -> m (TSEXP V Foreign.R.Type.Symbol) @-}
{-@ ignore install @-}
install :: MonadR m => String -> m (SEXP V)
install s = io (installIO s)

{-# DEPRECATED string, strings "Use mkSEXP instead" #-}

-- | Create an R character string from a Haskell string.
{-@ string :: String -> IO (TSEXP V Foreign.R.Type.SChar) @-}
string :: String -> IO (SEXP V)
string str = withCString str R.mkChar

-- | Create an R string vector from a Haskell string.
{-@ strings :: String -> IO (TSEXP V Foreign.R.Type.SString) @-}
strings :: String -> IO (SEXP V)
strings str = withCString str R.mkString

-- | Evaluate a (sequence of) expression(s) in the given environment, returning the
-- value of the last.
{-@ assume evalEnv :: SEXP s -> TSEXP s Foreign.R.Type.Env -> m (SEXP (Region m)) @-}
{-@ ignore evalEnv @-}
evalEnv :: MonadR m => SEXP s -> SEXP s -> m (SEXP (Region m))
evalEnv (hexp -> Language.R.HExp.Expr _ v) rho = acquire =<< do
    io $ alloca $ \p -> do
      mapM_ (\s -> void $ R.protect s) (Vector.toList v)
      x <- Prelude.last <$> forM (Vector.toList v) (\s -> do
          z <- R.tryEvalSilent s (R.release rho) p
          e <- peek p
          when (e /= 0) $ runRegion $ throwR rho
          return z)
      R.unprotect (Vector.length v)
      return x
evalEnv x rho = acquire =<< do
    io $ alloca $ \p -> R.withProtected (return (R.release x)) $ \_ -> do
      v <- R.tryEvalSilent x rho p
      e <- peek p
      when (e /= 0) $ runRegion $ throwR rho
      return v

-- | Evaluate a (sequence of) expression(s) in the global environment.
eval :: MonadR m => SEXP s -> m (SEXP (Region m))
eval x = evalEnv x (R.release globalEnv)

-- | Silent version of 'eval' function that discards it's result.
eval_ :: MonadR m => SEXP s -> m ()
eval_ = void . eval

-- | Throw an R error as an exception.
{-@ throwR :: TSEXP s Foreign.R.Type.Env -> m a @-}
throwR :: MonadR m => R.SEXP s   -- ^ Environment in which to find error.
       -> m a
throwR env = getErrorMessage env >>= io . throwIO . R.RError

-- | Cancel any ongoing R computation in the current process. After interruption
-- an 'RError' exception will be raised.
--
-- This call is safe to run in any thread. If there is no R computation running,
-- the next computaion will be immediately cancelled. Note that R will only
-- interrupt computations at so-called "safe points" (in particular, not in the
-- middle of a C call).
{-@ ignore cancel @-}
cancel :: IO ()
cancel = poke R.interruptsPending 1

-- | Throw an R exception with specified message.
throwRMessage :: MonadR m => String -> m a
throwRMessage = io . throwIO . R.RError

-- | Read last error message.
{-@ assume getErrorMessage :: TSEXP s Foreign.R.Type.Env -> m String @-}
{-@ ignore getErrorMessage @-}
getErrorMessage :: MonadR m => R.SEXP s -> m String
getErrorMessage e = io $ do
  R.withProtected (withCString "geterrmessage" ((R.install >=> R.lang1))) $ \f -> do
    R.withProtected (return (R.release e)) $ \env -> do
      peekCString
        =<< R.char
        =<< peek
        =<< R.string . R.checkSEXPTYPE R.SString
        =<< R.eval f env
