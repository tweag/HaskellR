-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low-level R functions. In this module, 'SEXP' values have to be
-- coerced unsafely because this module is lower in the abstraction stack than
-- "H.HExpr", so we can't use the type refinement facilities provided there.

{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.R
  ( r1
  , r2
  , parseFile
  , parseText
  , withProtected
  , install
  , string
  , strings
  , eval
  , evalEnv
  -- * R global constants
  -- $ghci-bug
  , pokeRVariables
  , peekRVariables
  , globalEnvPtr
  , baseEnvPtr
  , nilValuePtr
  , unboundValuePtr
  , missingArgPtr
  , rInteractive
  , rCStackLimitPtr
  , rInputHandlersPtr
  -- * Exceptions
  , throwR
  , throwRMessage
  -- * Helpers
  -- $helpers
  ) where

import Foreign.R (SEXP, SomeSEXP(..))
import qualified Foreign.R as R
import qualified Foreign.R.Parse as R
import qualified Foreign.R.Error as R
import qualified Foreign.R.Interface as R ( StackSize )

import Control.Applicative
import Control.Exception ( bracket, throwIO )
import Control.Monad ( (<=<), (>=>), when, unless )
import Data.ByteString as B
import Data.ByteString.Char8 as C8 ( pack, unpack )
import Foreign
    ( alloca
    , castPtr
    , peek
    , Ptr
    , poke
    , newStablePtr
    , deRefStablePtr
    , StablePtr
    )
import Foreign.C.String ( withCString, peekCString )
import Foreign.C.Types ( CInt(..) )
import System.IO.Unsafe ( unsafePerformIO )

-- $ghci-bug
-- The main reason to have all R constants referenced with a StablePtr
-- is that variables in shared libraries are linked incorrectly by GHCi with
-- loaded code.
--
-- The workaround is to grab all variables in the ghci session for the loaded
-- code to use them, that is currently done by the H.ghci script.
--
-- Upstream ticket: <https://ghc.haskell.org/trac/ghc/ticket/8549#ticket>

type RVariables =
    ( Ptr (SEXP R.Env)
    , Ptr (SEXP R.Env)
    , Ptr (SEXP R.Nil)
    , Ptr (SEXP R.Symbol)
    , Ptr (SEXP R.Symbol)
    , Ptr CInt
    , Ptr R.StackSize
    , Ptr (Ptr ())
    )

-- | Stores R variables in a static location. This makes the variables'
-- addresses accesible after reloading in GHCi.
pokeRVariables :: RVariables -> IO ()
pokeRVariables = poke rVariables <=< newStablePtr

-- | Retrieves R variables.
peekRVariables :: RVariables
peekRVariables = unsafePerformIO $ peek rVariables >>= deRefStablePtr

(  globalEnvPtr
 , baseEnvPtr
 , nilValuePtr
 , unboundValuePtr
 , missingArgPtr
 , rInteractive
 , rCStackLimitPtr
 , rInputHandlersPtr
 ) = peekRVariables

foreign import ccall "missing_r.h &" rVariables :: Ptr (StablePtr RVariables)

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
        eval expr

-- $helpers
-- This section contains a bunch of functions that are used internally on
-- a low level and wraps are simple that are too cheap to run under high
-- level interface.

-- | Call a pure unary R function of the given name in the global environment.
--
-- This function is here mainly for testing purposes.
r1 :: ByteString -> SEXP a -> SomeSEXP
r1 fn a =
    unsafePerformIO $
      useAsCString fn $ \cfn -> R.install cfn >>= \f ->
        withProtected (R.lang2 f a) eval

-- | Call a pure binary R function. See 'r1' for additional comments.
r2 :: ByteString -> SEXP a -> SEXP b -> SomeSEXP
r2 fn a b =
    unsafePerformIO $
      useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang3 f a b) eval

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: IO (SEXP a)      -- Action to acquire resource
              -> (SEXP a -> IO b) -- Action
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
parseFile :: FilePath -> (SEXP R.Expr -> IO a) -> IO a
parseFile fl f = do
    withCString fl $ \cfl ->
      withProtected (R.mkString cfl) $ \rfl ->
        case r1 (C8.pack "parse") rfl of
          R.SomeSEXP s -> return (R.unsafeCoerce s) `withProtected` f

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
install :: String -> IO (SEXP R.Symbol)
install str = withCString str R.install

-- | Create an R character string from a Haskell string.
string :: String -> IO (SEXP R.Char)
string str = withCString str R.mkChar

-- | Create an R string vector from a Haskell string.
strings :: String -> IO (SEXP R.String)
strings str = withCString str R.mkString

-- | Evaluate an expression in the given environment.
evalEnv :: SEXP a -> SEXP R.Env -> IO SomeSEXP
evalEnv x rho =
    alloca $ \p -> do
        v <- R.tryEvalSilent x rho p
        e <- peek p
        when (e /= 0) $ do
          throwR rho
        return v

-- | Evaluate an expression in the global environment.
eval :: SEXP a -> IO SomeSEXP
eval x = peek globalEnvPtr >>= evalEnv x

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
