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
  , rsafe
  , collectAntis
  ) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Data.Vector.SEXP as Vector
import qualified Foreign.R as R
import qualified Foreign.R.Parse as R
import           Foreign.R (SEXP, SomeSEXP(..))
import           Foreign.R.Error
import           Internal.Error
import           Language.R (eval)
import           Language.R.Globals (nilValue, globalEnv)
import           Language.R.GC (automaticSome)
import           Language.R.HExp
import           Language.R.Instance
import           Language.R.Literal (mkSEXPIO)

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isSuffixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import Foreign (alloca, peek)
import Foreign.C.String (withCString)
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Heredoc as Heredoc
import qualified System.IO.Temp as Temp
import System.Process
import System.IO
import System.Exit

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

-- | Quasiquoter for pure R code (no side effects) and that does not depend on
-- the global environment (referential transparency). This means that all
-- symbols must appear qualified with a package namespace (whose bindings are
-- locked by default), the code must not affect R shared state in any way,
-- including the global environment, and must not perform I/O.

-- TODO some of the above invariants can be checked statically. Do so.
rsafe :: QuasiQuoter
rsafe = QuasiQuoter
    { quoteExp  = \txt -> [| unsafePerformIO $ runRegion $ automaticSome =<< eval =<< $(expQQ txt) |]
    , quotePat  = unimplemented "quotePat"
    , quoteType = unimplemented "quoteType"
    , quoteDec  = unimplemented "quoteDec"
    }

-- | Serialize quasiquotes using a global lock, because the compiler is allowed
-- in theory to run them in parallel, yet the R runtime is not reentrant.
qqLock :: MVar ()
qqLock = unsafePerformIO $ newMVar ()
{-# NOINLINE qqLock #-}

parse :: String -> IO (R.SEXP V 'R.Expr)
parse txt = do
    initialize defaultConfig
    withCString txt $ \ctxt ->
        R.withProtected (R.mkString ctxt) $ \rtxt ->
          alloca $ \status -> do
            R.withProtected (R.parseVector rtxt (-1) status (R.release nilValue)) $ \exprs -> do
              rc <- fromIntegral <$> peek status
              unless (R.PARSE_OK == toEnum rc) $
                throwIO . RError $ "Parse error in: " ++ txt
              return exprs

antiSuffix :: String
antiSuffix = "_hs"

-- | Chop antiquotation variable names to get the corresponding Haskell variable name.
chop :: String -> String
chop name = take (length name - length antiSuffix) name

-- | Map backwards slashes to forward slashes.
#ifdef mingw32_HOST_OS
fixwinslash :: String -> String
fixwinslash str = let
  repl '\\' = '/'
  repl c = c
  in map repl str
#endif

-- | Find all occurences of antiquotations.
--
-- This function works by parsing the user's R code in a separate
-- R process. As a nice side-effect, it will detect and return any syntax
-- errors in the quasi-quoted R code.
--
-- This function is exposed only for testing; you probably don't need to
-- call it in the user code.
collectAntis
  :: String
    -- ^ the R code that may contain antiquotations, which are
    -- identifiers ending with 'antiSuffix'
  -> IO (Either String [String])
    -- ^ either an error message from R, or a list of unique antiquoted
    -- identifiers
collectAntis input = do
  -- Write our input to a temporary file. We could interpolate it into the
  -- R code below directly, but that would make it harder to disentangle
  -- syntax errors in the user's code from our wrapper code.
  Temp.withSystemTempFile "inline-r-.R" $ \input_file input_fh -> do
    hPutStr input_fh input
    hClose input_fh
    (code, stdout, stderr) <- readProcessWithExitCode "R" ["--slave"]
      -- Note: --slave was recently renamed to --no-echo. --slave still works
      -- but is no longer documented. Using the old option name for now just
      -- in case the user have an older (pre-2020) version of R.
      --                              
      -- Change backslashes to forward slashes in tempFile names 
      -- under Windows. Windows is tolerant of this Unixification, but 
      -- Unix systems (and R) are less tolerant of naked backslashes 
      -- outside of valid escape sequences. For example, 
      -- str <- "C:\Users\joe" is invalid in R.
#ifdef mingw32_HOST_OS
      $ "input_file <- \"" ++ (fixwinslash input_file) ++ "\"\n" ++
#else
      $ "input_file <- \"" ++ input_file ++ "\"\n" ++
#endif
        [Heredoc.there|R/collectAntis.R|]
    return $ case code of
      ExitSuccess -> Right $ words stdout
      ExitFailure{} -> Left stderr

-- | An R quasiquote is syntactic sugar for a function that we
-- generate, which closes over all antiquotation variables, and applies the
-- function to the Haskell values to which those variables are bound. Example:
--
-- @
-- [r| x_hs + y_hs |] ==> apply (apply [r| function(x_hs, y_hs) x_hs + y_hs |] x) y
-- @
expQQ :: String -> Q TH.Exp
expQQ input = do
    mb_antis <- liftIO $ collectAntis input
    antis <- case mb_antis of
      Right antis -> pure antis
      Left msg -> fail . unlines $
        [ "An error occurred while trying to parse the R code."
        , "The stderr of the R interpreter was:"
        , msg
        ]
    let args = map (TH.dyn . chop) antis
        closure = "function(" ++ intercalate "," antis ++ "){" ++ input ++ "}"
        z = [| return (R.release nilValue) |]
    vars <- mapM (\_ -> TH.newName "x") antis
    -- Abstract over antis using fresh vars, to avoid captures with names bound
    -- internally (such as 'f' below).
    x <- (\body -> foldl TH.appE body args) $ TH.lamE (map TH.varP vars)
      [| -- Memoize the runtime parsing of the generated closure (provided the
         -- compiler notices that it can let-float to top-level).
         let sx = unsafePerformIO $ do
                    exprs <- parse closure
                    SomeSEXP e <- R.readVector exprs 0
                    clos <- R.eval e (R.release globalEnv)
                    R.unSomeSEXP clos R.preserveObject
                    return clos
         in io $ case sx of
           SomeSEXP f ->
             R.lcons f =<<
               $(foldr (\x xs -> [| R.withProtected $xs $ \cdr -> do
                                        car <- mkSEXPIO $(TH.varE x)
                                        R.lcons car cdr |]) z vars)
       |]
    pure x
