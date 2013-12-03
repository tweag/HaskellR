-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low level R functions
module Language.R
  ( r1
  , r2
  , parseFile
  , parseText
  , withProtected
  , symbol
  , install
  , string
  , strings
  , eval
  -- * R global constants
  -- $ghci-bug
  , globalEnv
  , baseEnv
  , nilValue
  , unboundValue
  , missingArg
  ) where


import Control.Applicative
import Control.Exception ( bracket )
import Control.Monad ( (<=<), when, unless )
import Data.ByteString as B
import Data.ByteString.Char8 as C8 ( pack, unpack )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.Word
import Foreign ( alloca, nullPtr, peek )
import Foreign.C.String ( withCString )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Foreign.R as R
import qualified Foreign.R.Parse as R
import qualified Foreign.R.Error as R

-- $ghci-bug
-- The main reason to have all constant be presented as IORef in a global
-- scope is that peeking variable in ghci doesn't work as excepted an
-- returns incorrect address. The workaround is to populate all variables
-- in the ghci session, that is done automatically by the .ghci script.
--
-- Upstream ticket: <https://ghc.haskell.org/trac/ghc/ticket/8549#ticket>

globalEnv :: IORef (R.SEXP R.Env)
globalEnv = unsafePerformIO $ newIORef nullPtr

baseEnv :: IORef (R.SEXP R.Env)
baseEnv = unsafePerformIO $ newIORef nullPtr

nilValue :: IORef (R.SEXP R.Nil)
nilValue = unsafePerformIO $ newIORef nullPtr

unboundValue :: IORef (R.SEXP R.Symbol)
unboundValue = unsafePerformIO $ newIORef nullPtr

missingArg :: IORef (R.SEXP R.Symbol)
missingArg = unsafePerformIO $ newIORef nullPtr

-- | Parse and then evaluate expression.
parseEval :: ByteString -> IO (R.SEXP a)
parseEval txt = useAsCString txt $ \ctxt ->
  withProtected (R.mkString ctxt) $ \rtxt ->
    alloca $ \status -> do
      nil <- readIORef nilValue
      z <- withProtected (R.parseVector rtxt 1 status nil) $ \ex -> 
             eval =<< R.indexExpr ex 0
      e <- fromIntegral <$> peek status
      unless (R.PARSE_OK == toEnum e) $
        R.throwRMessage $ "Parse error in: " ++ C8.unpack txt
      return z

-- | Call 1-arity R function by name, function will be found in runtime,
-- using global environment, no additional environment is provided to
-- function.
--
-- This function is done mainly for testing purposes, and execution of R
-- code in case that we can't construct symbol by other methods.
r1 :: ByteString -> R.SEXP a -> R.SEXP b
r1 fn a =
    unsafePerformIO $
      useAsCString fn $ \cfn -> R.install cfn >>= \f -> do
        withProtected (R.lang2 f a) eval

-- | Call 2-arity R function, function will be found in runtime, using
-- global environment. See 'r1' for additional comments.
r2 :: ByteString -> R.SEXP a -> R.SEXP b -> R.SEXP c
r2 fn a b =
    unsafePerformIO $
      useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang3 f a b) eval

-- | Perform an action with resource while protecting it from the garbage
-- collection.
withProtected :: IO (R.SEXP a)      -- Action to accure resource
              -> (R.SEXP a -> IO b) -- Action
              -> IO b
withProtected accure =
   bracket (accure >>= \x -> R.protect x >> return x)
           (const (R.unprotect 1))

-- | Parse file and perform some actions on parsed file.
--
-- This function uses continuation because this is an easy way to make
-- operations GC-safe.
--
-- This function is not safe to use inside GHCi.
parseFile :: FilePath -> (R.SEXP (R.Vector (R.SEXP R.Any)) -> IO a) -> IO a
parseFile fl f = do
    withCString fl $ \cfl ->
      withProtected (R.mkString cfl) $ \rfl ->
        withProtected (return $ r1 (C8.pack "parse") rfl) f

parseText :: String -> IO (R.SEXP a)
parseText txt = parseEval (C8.pack $ "parse(text="++show txt++")")

install :: String -> IO (R.SEXP R.Symbol)
install str = withCString str (R.protect <=< R.install)

symbol :: String -> IO (R.SEXP R.Symbol)
symbol str = do
    gl <- readIORef globalEnv
    withCString str $ \cstr ->
      withProtected (R.install cstr) $
        flip R.findVar gl

string :: String -> IO (R.SEXP (R.Vector Word8))
string str = withCString str (R.protect <=< R.mkChar)

strings :: String -> IO (R.SEXP (R.String))
strings str = withCString str (R.protect <=< R.mkString)

eval :: R.SEXP a -> IO (R.SEXP b)
eval x = do
    gl <- readIORef globalEnv
    alloca $ \p -> do
        v <- R.tryEvalSilent x gl p
        e <- peek p
        when (e /= 0) $ do
          R.throwR gl
        return v
