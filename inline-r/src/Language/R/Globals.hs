{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Global variables used by the R interpreter. All are constant, but the values
-- of some of them may change over time (e.g. the global environment).

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}

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
import Foreign.C -- only needed to help name resolution in LH
import Foreign.R (SEXP)
import qualified Foreign.R as R -- only needed to help name resolution in LH
#ifndef mingw32_HOST_OS
import qualified Foreign.R.EventLoop as R
#endif
import System.IO.Unsafe (unsafePerformIO)

_ = undefined :: (Foreign.C.CString, R.SEXP s)

-- Turn off complaints about peek and poke preconditions
{-@ assume peek :: Ptr a -> IO a @-}
{-@ assume poke :: Ptr a -> a -> IO () @-}

-- $ghci-bug
-- The main reason to have all R constants referenced with a StablePtr
-- is that variables in shared libraries are linked incorrectly by GHCi with
-- loaded code.
--
-- The workaround is to grab all variables in the ghci session for the loaded
-- code to use them, that is currently done by the H.ghci script.
--
-- Upstream ticket: <https://ghc.haskell.org/trac/ghc/ticket/8549#ticket>

{-@
type RVariables =
    ( Ptr (TSEXP G Env)
    , Ptr (TSEXP G Env)
    , Ptr (TSEXP G Env)
    , Ptr (TSEXP G Nil)
    , Ptr (TSEXP G Symbol)
    , Ptr (TSEXP G Symbol)
    , Ptr CInt
    , Ptr CInt
#ifndef mingw32_HOST_OS
    , Ptr (Ptr R.InputHandler)
#endif
    )
@-}

type RVariables =
    ( Ptr (SEXP G)
    , Ptr (SEXP G)
    , Ptr (SEXP G)
    , Ptr (SEXP G)
    , Ptr (SEXP G)
    , Ptr (SEXP G)
    , Ptr CInt
    , Ptr CInt
#ifndef mingw32_HOST_OS
    , Ptr (Ptr R.InputHandler)
#endif
    )

-- | Stores R variables in a static location. This makes the variables'
-- addresses accesible after reloading in GHCi.
foreign import ccall "missing_r.h &" rVariables :: Ptr (StablePtr RVariables)

{-@ assume pokeRVariables :: RVariables -> IO () @-}
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

{-@ assume unboundValue :: TSEXP G Symbol @-}
-- | Special value to which all symbols unbound in the current environment
-- resolve to.
unboundValue :: SEXP G
unboundValue = unsafePerformIO $ peek unboundValuePtr

{-@ assume nilValue :: TSEXP G Nil @-}
-- | R's @NULL@ value.
nilValue :: SEXP G
nilValue = unsafePerformIO $ peek nilValuePtr

{-@ assume missingArg :: TSEXP G Symbol @-}
-- | Value substituted for all missing actual arguments of a function call.
missingArg :: SEXP G
missingArg = unsafePerformIO $ peek missingArgPtr

{-@ assume baseEnv :: TSEXP G Env @-}
-- | The base environment.
baseEnv :: SEXP G
baseEnv = unsafePerformIO $ peek baseEnvPtr

{-@ assume emptyEnv :: TSEXP G Env @-}
-- | The empty environment.
emptyEnv :: SEXP G
emptyEnv = unsafePerformIO $ peek emptyEnvPtr

{-@ assume globalEnv :: TSEXP G Env @-}
-- | The global environment.
globalEnv :: SEXP G
globalEnv = unsafePerformIO $ peek globalEnvPtr

#ifndef mingw32_HOST_OS
inputHandlers :: Ptr R.InputHandler
inputHandlers = unsafePerformIO $ peek inputHandlersPtr
#endif
