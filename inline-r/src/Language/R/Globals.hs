{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Global variables used by the R interpreter. All are constant, but the values
-- of some of them may change over time (e.g. the global environment).

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.R.Globals
  ( baseEnv
  , emptyEnv
  , globalEnv
  , nilValue
  , missingArg
  , unboundValue
  -- * R Internal constants
  , isRInteractive
  , signalHandlersPtr
#ifndef mingw32_HOST_OS
  , inputHandlers
#endif
  -- * R global constants
  -- $ghci-bug
  , pokeRVariables
  ) where

import Control.Memory.Region
import Control.Monad ((<=<))
import Foreign
    ( Ptr
    , StablePtr
    , deRefStablePtr
    , newStablePtr
    , peek
    , poke
    )
import Foreign.C.Types (CInt)
import Foreign.R (SEXP)
import qualified Foreign.R as R
#ifndef mingw32_HOST_OS
import qualified Foreign.R.EventLoop as R
#endif
import System.IO.Unsafe (unsafePerformIO)

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
    ( Ptr (SEXP G 'R.Env)
    , Ptr (SEXP G 'R.Env)
    , Ptr (SEXP G 'R.Env)
    , Ptr (SEXP G 'R.Nil)
    , Ptr (SEXP G 'R.Symbol)
    , Ptr (SEXP G 'R.Symbol)
    , Ptr CInt
    , Ptr CInt
#ifndef mingw32_HOST_OS
    , Ptr (Ptr R.InputHandler)
#endif
    )

-- | Stores R variables in a static location. This makes the variables'
-- addresses accesible after reloading in GHCi.
foreign import ccall "missing_r.h &" rVariables :: Ptr (StablePtr RVariables)

pokeRVariables :: RVariables -> IO ()
pokeRVariables = poke rVariables <=< newStablePtr

(  baseEnvPtr
 , emptyEnvPtr
 , globalEnvPtr
 , nilValuePtr
 , unboundValuePtr
 , missingArgPtr
 , isRInteractive
 , signalHandlersPtr
#ifndef mingw32_HOST_OS
 , inputHandlersPtr
#endif
 ) = unsafePerformIO $ peek rVariables >>= deRefStablePtr

-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP G 'R.Symbol
unboundValue = unsafePerformIO $ peek unboundValuePtr

-- | R's @NULL@ value.
nilValue :: SEXP G 'R.Nil
nilValue = unsafePerformIO $ peek nilValuePtr

-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP G 'R.Symbol
missingArg = unsafePerformIO $ peek missingArgPtr

-- | The base environment.
baseEnv :: SEXP G 'R.Env
baseEnv = unsafePerformIO $ peek baseEnvPtr

-- | The empty environment.
emptyEnv :: SEXP G 'R.Env
emptyEnv = unsafePerformIO $ peek emptyEnvPtr

-- | The global environment.
globalEnv :: SEXP G 'R.Env
globalEnv = unsafePerformIO $ peek globalEnvPtr

#ifndef mingw32_HOST_OS
inputHandlers :: Ptr R.InputHandler
inputHandlers = unsafePerformIO $ peek inputHandlersPtr
#endif
