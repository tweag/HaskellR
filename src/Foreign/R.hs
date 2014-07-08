-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to R core datatypes and functions. Nearly all structures
-- allocated internally in R are instances of a 'SEXPREC'. A pointer to
-- a 'SEXPREC' is called a 'SEXP'.
--
-- To allow for precise typing of bindings to primitive R functions, we index
-- 'SEXP's by 'SEXPTYPE', which classifies the /form/ of a 'SEXP' (see
-- "Foreign.R.Type"). A function accepting 'SEXP' arguments of any type should
-- leave the type index uninstantiated. A function returning a 'SEXP' result of
-- unknown type should use 'SomeSEXP'. (More precisely, unknown types in
-- /negative/ position should be /universally/ quantified and unknown types in
-- /positive/ position should be /existentially/ quantified).
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R
  ( -- * Internal R structures
    SEXPTYPE(..)
  , Internal.Logical(..)
  , SEXP(..)
  , SomeSEXP(..)
  , Callback
  , unSomeSEXP
  , unsafeWrap
  , unsafeWrapSome
  , unwrap
  , unwrapSome
    -- * Casts and coercions
    -- $cast-coerce
  , cast
  , asTypeOf
  , unsafeCoerce
    -- * Node creation
    -- Concider using 'Language.R.HExp.unhexp'
  , allocSEXP
  , allocList
  , cons
  , install
  , mkString
  , mkChar
  , CEType(..)
  , mkCharCE
  , mkWeakRef
    -- * Node attributes
    -- There is no highlevel API yet
  , typeOf
  , setAttribute
  , getAttribute
    -- * Evaluation
  , eval
  , tryEval
  , tryEvalSilent
  , lang1
  , lang2
  , lang3
  , findFun
  , findVar
    -- * GC functions
  , gc
    -- * Globals
  , globalEnv
  , baseEnv
  , nilValue
  , rInputHandlers

    -- * Communication with runtime
  , printValue
  , processEvents
#ifdef H_ARCH_UNIX
  , processGUIEventsUnix
#endif
  -- * Internal types and functions
  --
  -- | Should not be used in user code. These exports are only needed for
  -- binding generation tools.
  , sexp
  , unsexp
  -- * Regions
  , LocalRegion
  , wrapLocal
  , CallbackRegion
  , wrapCallback
  , protect
  , protectSome
  , withProtected
  ) where

import           Control.Monad.R (R(..), unsafeIOToR)
import qualified Control.Monad.R as MonadR
import           Control.Monad.R.Class
import           Foreign.R.Internal (SEXPTYPE(..), SSEXPTYPE)
import           Foreign.R.Internal ( CEType )
import qualified Foreign.R.Internal as Internal

import Control.Applicative
import Control.Monad.Catch ( MonadMask, MonadCatch, bracket )
import Control.Monad.Reader
import Foreign (Ptr, castPtr, Storable(..))
#ifdef H_ARCH_WINDOWS
import Foreign (nullPtr)
#endif
import Foreign.C
import Prelude hiding (asTypeOf, length)

--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

-- | The basic type of all R expressions, classified by the form of the
-- expression.
newtype SEXP s (a :: SEXPTYPE) = SEXP { unSEXP :: Internal.SEXP a }

instance Eq (SEXP s a) where
  SEXP a == SEXP b = a == b

instance Show (SEXP s a) where
  show (SEXP ptr) = show ptr

instance Storable (SEXP s a) where
  sizeOf _ = sizeOf (undefined :: Ptr (SEXP s a))
  alignment _ = alignment (undefined :: Ptr (SEXP s a))
  peek ptr = SEXP <$> peek (castPtr ptr)
  poke ptr (SEXP s) = poke (castPtr ptr) s
  
-- | 'SEXP' with no type index. This type and 'sexp' / 'unsexp'
-- are purely an artifact of c2hs (which doesn't support indexing a Ptr with an
-- arbitrary type in a @#pointer@ hook).

-- | Add a type index to the pointer.
sexp :: Internal.SEXP0 -> SEXP s a
sexp = SEXP . castPtr

unsexp :: SEXP s a -> Internal.SEXP0
unsexp = Internal.unsexp . unSEXP

-- | Unsafe convertion from untyped to typed one
unsafeWrap :: Internal.SEXP a -> SEXP s a
unsafeWrap s = SEXP (castPtr s)

unsafeWrapSome :: Internal.SomeSEXP -> SomeSEXP s
unsafeWrapSome (Internal.SomeSEXP s) = SomeSEXP (unsafeWrap s)

unwrap :: SEXP s a -> Internal.SEXP a
unwrap (SEXP s) = (castPtr s)

unwrapSome :: SomeSEXP s -> Internal.SomeSEXP
unwrapSome (SomeSEXP s) = Internal.SomeSEXP (unwrap s)

-- | A 'SEXP' of unknown form.
data SomeSEXP s = forall a. SomeSEXP {-# UNPACK #-} !(SEXP s a)

instance Storable (SomeSEXP s) where
  sizeOf _ = sizeOf (undefined :: SEXP s a)
  alignment _ = alignment (undefined :: SEXP s a)
  peek ptr = SomeSEXP <$> peek (castPtr ptr)
  poke ptr (SomeSEXP s) = poke (castPtr ptr) s

-- | Deconstruct a 'SomeSEXP'. Takes a continuation since otherwise the
-- existentially quantified variable hidden inside 'SomeSEXP' would escape.
unSomeSEXP :: SomeSEXP s -> (forall a. SEXP s a -> r) -> r
unSomeSEXP (SomeSEXP s) k = k s

-- | Foreign functions are represented in R as external pointers. We call these
-- "callbacks", because they will typically be Haskell functions passed as
-- arguments to higher-order R functions.
type Callback s = SEXP s ExtPtr

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | Return the \"type\" tag (aka the form tag) of the given 'SEXP'. This
-- function is pure because the type of an object does not normally change over
-- the lifetime of the object.
typeOf :: SEXP s a -> SEXPTYPE
typeOf = Internal.typeOf . unSEXP

--------------------------------------------------------------------------------
-- Coercion functions                                                         --
--------------------------------------------------------------------------------

-- $cast-coerce
--
-- /Coercions/ have no runtime cost, but are completely unsafe. Use with
-- caution, only when you know that a 'SEXP' is of the target type. /Casts/ are
-- safer, but introduce a runtime type check. The difference between the two is
-- akin to the difference between a C-style typecasts and C++-style
-- @dynamic_cast@'s.

-- | Cast the type of a 'SEXP' into another type. This function is partial: at
-- runtime, an error is raised if the source form tag does not match the target
-- form tag.
cast :: SEXPTYPE -> SomeSEXP s -> SEXP s b
cast ty (SomeSEXP s) =
  SEXP . Internal.cast ty . Internal.SomeSEXP . unSEXP $ s

-- | Cast form of first argument to that of the second argument.
asTypeOf :: SomeSEXP s -> SEXP s a -> SEXP s a
asTypeOf (SomeSEXP (SEXP s)) (SEXP s') = SEXP $ Internal.asTypeOf (Internal.SomeSEXP s)  s'

-- | Unsafe coercion from one form to another. This is unsafe, in the sense that
-- using this function improperly could cause code to crash in unpredictable
-- ways. Contrary to 'cast', it has no runtime cost since it does not introduce
-- any dynamic check at runtime.
unsafeCoerce :: SEXP s a -> SEXP s b
unsafeCoerce = SEXP . castPtr . unSEXP

--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Initialize a new string vector.
mkString :: CString -> R s (SEXP s Internal.String)
mkString = protect <=< liftIO . Internal.mkString

-- | Initialize a new character vector (aka a string).
mkChar :: CString -> R s (SEXP s Internal.Char)
mkChar = protect <=< liftIO . Internal.mkChar

-- | Create Character value with specified encoding
mkCharCE :: CString -> CEType -> R s (SEXP s Internal.Char)
mkCharCE s t = protect =<< liftIO (Internal.mkCharCE s t)

-- | Probe the symbol table
install :: CString -> R s (SEXP s Internal.Symbol)
install = protect <=< liftIO  . Internal.install

-- | Allocate a 'SEXP'.
allocSEXP :: SSEXPTYPE a -> R s (SEXP s a)
allocSEXP = protect <=< liftIO . Internal.allocSEXP

-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: Int -> R s (SEXP s Internal.List)
allocList = protect <=< liftIO . Internal.allocList

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: SEXP s a -> SEXP s b -> R s (SEXP s Internal.List)
cons a b = protect =<< liftIO (Internal.cons (unSEXP a) (unSEXP b))

mkWeakRef :: SEXP s a -> SEXP s b -> SEXP s c -> Bool -> R s (SEXP s Internal.WeakRef)
mkWeakRef a b c d = protect =<< liftIO (Internal.mkWeakRef (unSEXP a) (unSEXP b) (unSEXP c) d)

--
-- | Print a string representation of a 'SEXP' on the console.
printValue :: SEXP s a -> R s ()
printValue = unsafeIOToR . Internal.printValue . unSEXP

-- | Function for processing GUI and other events in the internal event loop.
processEvents :: R s ()
processEvents = unsafeIOToR Internal.processEvents

#ifdef H_ARCH_UNIX
processGUIEventsUnix :: Ptr (Ptr ()) -> R s ()
processGUIEventsUnix = unsafeIOToR . Internal.processGUIEventsUnix
#endif

-- | Invoke an R garbage collector sweep.
gc :: R s ()
gc = unsafeIOToR Internal.gc

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
eval :: SEXP s' a -> SEXP s'' Env -> R s (SomeSEXP s)
eval a b = protectSome <=< unsafeIOToR $ Internal.eval (unSEXP a) (unSEXP b)

-- | Try to evaluate expression.
tryEval :: SEXP s' a -> SEXP s'' Env -> Ptr CInt -> R s (SomeSEXP s)
tryEval a b c = protectSome <=< unsafeIOToR $ Internal.tryEval (unSEXP a) (unSEXP b) c

-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: SEXP s' a -> SEXP s'' Env -> Ptr CInt -> R s (SomeSEXP s)
tryEvalSilent a b c = protectSome <=< unsafeIOToR $ Internal.tryEvalSilent (unSEXP a) (unSEXP b) c

lang1 :: SEXP s a -> R s (SEXP s Internal.Lang)
lang1 = protect <=< liftIO . Internal.lang1 . unSEXP

lang2 :: SEXP s a -> SEXP s b ->  R s (SEXP s Internal.Lang)
lang2 a b = protect =<< liftIO (Internal.lang2 (unSEXP a) (unSEXP b))

lang3 :: SEXP s a -> SEXP s b -> SEXP s c -> R s (SEXP s Internal.Lang)
lang3 a b c = protect =<< liftIO (Internal.lang3 (unSEXP a) (unSEXP b) (unSEXP c))

-- | Find a function by name.
findFun :: SEXP s a -> SEXP s Env -> R s (SomeSEXP s)
findFun a e = protectSome =<< liftIO (Internal.findFun (unSEXP a) (unSEXP e))

-- | Find a variable by name.
findVar :: SEXP s a -> SEXP s Env -> R s (SEXP s Internal.Symbol)
findVar a e = protect =<< liftIO (Internal.findVar (unSEXP a) (unSEXP e))

--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

nilValue :: Ptr (SEXP s Nil)
nilValue = castPtr Internal.nilValue

globalEnv :: Ptr (SEXP s Env)
globalEnv = castPtr Internal.globalEnv


baseEnv :: Ptr (SEXP s Env)
baseEnv = castPtr Internal.baseEnv

-- | Input handlers used in event loops.
#ifdef H_ARCH_UNIX
foreign import ccall "&R_InputHandlers" rInputHandlers :: Ptr (Ptr ())
#else
rInputHandlers :: Ptr (Ptr ())
rInputHandlers = nullPtr
#endif

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

-- | Get the attribute list from the given object.
getAttribute :: SEXP s a -> R s (SEXP s b)
getAttribute = unsafeIOToR . fmap SEXP . Internal.getAttribute . unSEXP

-- | Set the attribute list.
setAttribute :: SEXP s a -> SEXP s b -> R s ()
setAttribute (SEXP a) (SEXP b) = unsafeIOToR $ Internal.setAttribute a b


data LocalRegion
data CallbackRegion

wrapLocal :: Internal.SEXP s -> SEXP LocalRegion s
wrapLocal = unsafeWrap

wrapCallback :: Internal.SEXP s -> SEXP CallbackRegion s
wrapCallback = unsafeWrap

protect :: Internal.SEXP a -> R s (SEXP s a)
protect = fmap SEXP . MonadR.protect

protectSome :: Internal.SomeSEXP -> R s (SomeSEXP s)
protectSome (Internal.SomeSEXP f) = fmap (SomeSEXP . SEXP) (MonadR.protect f)

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: (MonadR m, MonadIO m, MonadCatch m, MonadMask m)
              => m (SEXP s a)      -- Action to acquire resource
	      -> (SEXP s a -> m b) -- Action
	      -> m b
withProtected acquire f =
   bracket
     (acquire >>= io . Internal.protect . unSEXP)
     (const $ io $ Internal.unprotect 1)
     $ f . SEXP

