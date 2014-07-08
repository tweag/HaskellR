-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low-level R functions. In this module, 'SEXP' values have to be
-- coerced unsafely because this module is lower in the abstraction stack than
-- "H.HExpr", so we can't use the type refinement facilities provided there.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# Language ViewPatterns #-}
{-# Language GADTs #-}

module Language.R
  ( r1
  , r2
  , parseFile
  , parseText
  , install
  , installIO
  , string
  , strings
  , eval
  , evalEnv
  , eval_
  , evalIO
  -- * R global constants
  -- $ghci-bug
  , module Foreign.R.Runner
  , module Control.Monad.R
  -- * Exceptions
  , throwR
  , throwRMessage
  -- * Helpers
  -- $helpers
  , module Language.R.GC
  ) where

import qualified Data.Vector.SEXP as Vector
import Control.Monad.R.Class
import Control.Monad.R
import Foreign.R.Internal (SEXP, SomeSEXP(..))
import qualified Foreign.R.Internal as R
import qualified Foreign.R.Parse as R
import qualified Foreign.R.Error as R
import           Foreign.R.Runner
import           Language.R.GC
import           Language.R.HExp.Unsafe

import Control.Applicative
import Control.Exception ( throwIO )
import Control.Monad ( (>=>), when, unless, forM, void )
import Data.ByteString as B
import Data.ByteString.Char8 as C8 ( pack, unpack )
import Foreign
    ( alloca
    , castPtr
    , peek
    )
import Foreign.C.String ( withCString, peekCString )

-- | Parse and then evaluate expression.
parseEval :: ByteString -> IO SomeSEXP
parseEval txt = useAsCString txt $ \ctxt ->
  withProtected (R.mkString ctxt) $ \rtxt ->
    alloca $ \status -> do
      nil <- peek nilValuePtr
      withProtected (R.parseVector rtxt 1 status nil) $ \exprs -> do
        rc <- fromIntegral <$> peek status
        unless (R.PARSE_OK == toEnum rc) $
          throwRMessage $ "Parse error in: " ++ C8.unpack txt
        SomeSEXP expr <- peek $ castPtr $ R.unsafeSEXPToVectorPtr exprs
        evalIO expr

-- $helpers
-- This section contains a bunch of functions that are used internally on
-- a low level and wraps are simple that are too cheap to run under high
-- level interface.

-- | Call a pure unary R function of the given name in the global environment.
--
-- This function is here mainly for testing purposes.
r1 :: ByteString -> SEXP a -> IO SomeSEXP
r1 fn a =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang2 f a) evalIO

-- | Call a pure binary R function. See 'r1' for additional comments.
r2 :: ByteString -> SEXP a -> SEXP b -> IO SomeSEXP
r2 fn a b =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang3 f a b) evalIO

-- | Parse file and perform some actions on parsed file.
--
-- This function uses continuation because this is an easy way to make
-- operations GC-safe.
--
-- This function is not safe to use inside GHCi.
parseFile :: FilePath -> (SEXP R.Expr -> IO a) -> IO a
parseFile fl f = do
    withCString fl $ \cfl ->
      withProtected (R.mkString cfl) $ \rfl ->
        r1 (C8.pack "parse") rfl >>= \(R.SomeSEXP s) ->
          return (R.unsafeCoerce s) `withProtected` f

parseText :: String                               -- ^ Text to parse
          -> Bool                                 -- ^ Whether to annotate the
                                                  -- AST with source locations.
          -> IO (R.SEXP R.Expr)
parseText txt b = do
    s <- parseEval $ C8.pack $
         "parse(text=" ++ show txt ++ ", keep.source=" ++ keep ++ ")"
    return $ R.Expr `R.cast` s
  where
    keep | b         = "TRUE"
         | otherwise = "FALSE"

-- | Internalize a symbol name.
installIO :: String -> IO (SEXP R.Symbol)
installIO str = withCString str R.install

install :: MonadR m => String -> m (SEXP R.Symbol)
install = io . installIO

-- | Create an R character string from a Haskell string.
string :: String -> IO (SEXP R.Char)
string str = withCString str R.mkChar

-- | Create an R string vector from a Haskell string.
strings :: String -> IO (SEXP R.String)
strings str = withCString str R.mkString

-- | Evaluate an expression in the given environment.
evalEnvIO :: SEXP a -> SEXP R.Env -> IO SomeSEXP
evalEnvIO (hexp -> Expr _ v) rho = 
    alloca $ \p -> do
      mapM_ (\(SomeSEXP s) -> void $ R.protect s) (Vector.toList v)
      x <- Prelude.last <$> forM (Vector.toList v) (\(SomeSEXP s) -> do
          z <- R.tryEvalSilent s rho p
          e <- peek p
          when (e /= 0) $ throwR rho
          return z)
      R.unprotect (Vector.length v)
      return x
evalEnvIO x rho =
    alloca $ \p -> do
        v <- R.tryEvalSilent x rho p
        e <- peek p
        when (e /= 0) $ throwR rho
        return v

-- | Evaluate an expression in the global environment.
evalIO :: SEXP a -> IO SomeSEXP
evalIO x = peek globalEnvPtr >>= evalEnvIO x

evalEnv :: MonadR m => SEXP a -> SEXP R.Env -> m SomeSEXP
evalEnv = (io .). evalEnvIO

eval :: MonadR m => SEXP a -> m SomeSEXP
eval = io . evalIO

-- | Silent version of 'evalIO' function that discards it's result.
eval_ :: MonadR m => SEXP a -> m ()
eval_ = void . eval

-- | Throw an R error as an exception.
throwR :: R.SEXP R.Env                         -- ^ Environment in which to find error.
       -> IO a
throwR env = getErrMsg env >>= throwIO . R.RError

-- | Throw an R exception with specified message.
throwRMessage :: String -> IO a
throwRMessage = throwIO . R.RError

-- | Read last error message.
getErrMsg :: R.SEXP R.Env -> IO String
getErrMsg e = do
  f <- withCString "geterrmessage" (R.install >=> R.lang1)
  peekCString =<< R.char =<< peek =<< R.string . R.cast R.String =<< R.eval f e
