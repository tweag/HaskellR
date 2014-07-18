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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Foreign.R
  ( R
    -- * Internal R structures
  , SEXPTYPE(..)
  , Internal.Logical(..)
  , SEXP(..)
  , SomeSEXP(..)
  , Callback
  , unSomeSEXP
  , unsafeWrap
  , unsafeWrapSome
  , unwrap
  , unwrapSome
  -- * Regions
  -- , protect
  , liftProtect
  , withProtected
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
  , Protect(..)
  , Unprotect(..)
  ) where

import           Control.Monad.R.Unsafe (R, UnsafeValue, unsafeIOToR)
import qualified Control.Monad.R.Unsafe as Unsafe
import           Foreign.R.Internal (SEXPTYPE(..), SSEXPTYPE)
import           Foreign.R.Internal ( CEType )
import qualified Foreign.R.Internal as Internal

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Catch ( bracket )
import Control.Monad.Reader
import Data.Int
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

instance NFData (SEXP s a) where
  rnf (SEXP s) = rnf (Internal.RPtr s)

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

instance NFData (SomeSEXP s) where
  rnf (SomeSEXP s) = rnf s

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
mkString = liftProtect . Internal.mkString

-- | Initialize a new character vector (aka a string).
mkChar :: CString -> R s (SEXP s Internal.Char)
mkChar = liftProtect . Internal.mkChar

-- | Create Character value with specified encoding
mkCharCE :: CString -> CEType -> R s (SEXP s Internal.Char)
mkCharCE s t = liftProtect $ Internal.mkCharCE s t

-- | Probe the symbol table
install :: CString -> R s (SEXP s Internal.Symbol)
install = liftProtect . Internal.install

-- | Allocate a 'SEXP'.
allocSEXP :: SSEXPTYPE a -> R s (SEXP s a)
allocSEXP = liftProtect . Internal.allocSEXP

-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: Int -> R s (SEXP s Internal.List)
allocList = liftProtect . Internal.allocList

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: SEXP s a -> SEXP s b -> R s (SEXP s Internal.List)
cons a b = liftProtect $ Internal.cons (unSEXP a) (unSEXP b)

mkWeakRef :: SEXP s a -> SEXP s b -> SEXP s c -> Bool -> R s (SEXP s Internal.WeakRef)
mkWeakRef a b c d = liftProtect $ Internal.mkWeakRef (unSEXP a) (unSEXP b) (unSEXP c) d

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
eval a b = liftProtect $ Internal.eval (unSEXP a) (unSEXP b)

-- | Try to evaluate expression.
tryEval :: SEXP s' a -> SEXP s'' Env -> Ptr CInt -> R s (SomeSEXP s)
tryEval a b c = liftProtect $ Internal.tryEval (unSEXP a) (unSEXP b) c

-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: SEXP s' a -> SEXP s'' Env -> Ptr CInt -> R s (SomeSEXP s)
tryEvalSilent a b c = liftProtect $ Internal.tryEvalSilent (unSEXP a) (unSEXP b) c

lang1 :: SEXP s a -> R s (SEXP s Internal.Lang)
lang1 = liftProtect . Internal.lang1 . unSEXP

lang2 :: SEXP s a -> SEXP s b ->  R s (SEXP s Internal.Lang)
lang2 a b = liftProtect $ Internal.lang2 (unSEXP a) (unSEXP b)

lang3 :: SEXP s a -> SEXP s b -> SEXP s c -> R s (SEXP s Internal.Lang)
lang3 a b c = liftProtect $ Internal.lang3 (unSEXP a) (unSEXP b) (unSEXP c)

-- | Find a function by name.
findFun :: SEXP s a -> SEXP s Env -> R s (SomeSEXP s)
findFun a e = liftProtect $ Internal.findFun (unSEXP a) (unSEXP e)

-- | Find a variable by name.
findVar :: SEXP s a -> SEXP s Env -> R s (SEXP s Internal.Symbol)
findVar a e = liftProtect $ Internal.findVar (unSEXP a) (unSEXP e)

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

-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: R s (SEXP s a)      -- Action to acquire resource
	      -> (SEXP s a -> R s b) -- Action
	      -> R s b
withProtected acquire f =
   bracket
     (acquire >>= unsafeIOToR . Internal.protect . unSEXP)
     (const $ unsafeIOToR $ Internal.unprotect 1)
     $ f . SEXP

liftProtect :: Protect s a => IO a -> R s (ProtectElt s a)
liftProtect = unsafeIOToR >=> protect

-- | Values that can be protected in a region. Basically this typeclass
-- is created in order to have an overloaded protect function, so it can
-- be called on any value and still to the right thing.
--
-- There is a default values for 'ProtectElt' and 'protect' functions
-- that allowes to add an instances to arbitrary datatypes that shouldn't
-- be protected.
class Protect s a where
   -- | A new type that user will have after a protection
   type ProtectElt s a :: *
   type ProtectElt s a = a
   -- | Run a protect action. This method really protects value withing
   -- a block, old value is still safe to use, but you should not do it
   -- for safety reasons.
   protect   :: a -> R s (ProtectElt s a)
   default protect :: (ProtectElt s a ~ a) => a -> R s a
   protect = return
   {-# INLINE protect #-}

instance Protect s (Internal.SEXP a) where
   type ProtectElt s (Internal.SEXP a) = SEXP s a
   protect = fmap SEXP . Unsafe.protect

instance Protect s (Internal.SomeSEXP) where
   type ProtectElt s Internal.SomeSEXP = SomeSEXP s
   protect (Internal.SomeSEXP f) = fmap (SomeSEXP . SEXP) (Unsafe.protect f)

instance Protect s a => Protect s (UnsafeValue a) where
   type ProtectElt s (UnsafeValue a)   = ProtectElt s a
   protect a = Unsafe.unsafeUseValue a protect

instance Protect s (SEXP s a) where
   type ProtectElt s (SEXP s a)        = SEXP s a
   protect = return

instance Protect s (SomeSEXP s) where
   type ProtectElt s (SomeSEXP s)      = SomeSEXP s
   protect = return

-- | Values that can be returned from a protection block.
class Unprotect a where
  -- | Type that describes the type that value will have after unprotection
  -- procedure. This type should have following properties:
  --
  --   1. Protection information should be erased (as value is no longer protected withing a region).
  --
  --   2. If a value is not safe to use (i.e. it should be protected) it should be wrapped with
  --   'Unsafe.UnsafeValue'. So user will know that he should protect a variable before use.
  type UnprotectElt a :: *
  type UnprotectElt a = a
  -- | Prepare a variable for the leaving region block. This function only conver a
  -- variable into acceptable type but doesn't perform any unprotection procedures.
  --
  -- Required properties:
  --
  --   1. The value should guarantee that there is no references to region variables exists in
  --   thunks, the easiest way to ensure is to fully evaluate an output value.
  --
  --   2. This function is made monadic in order to guarantee that it will be executed in order.
  unprotect :: a -> R s (UnprotectElt a)
  default unprotect :: (NFData a, UnprotectElt a ~ a) => a -> R s a
  unprotect x = rnf x `seq` return x
  {-# INLINE unprotect #-}

instance Unprotect (Internal.SEXP a) where
   type UnprotectElt (Internal.SEXP a) = UnsafeValue (Internal.SEXP a)
   unprotect = return . Unsafe.mkUnsafe

instance Unprotect (Internal.SomeSEXP) where
   type UnprotectElt (Internal.SomeSEXP)    = UnsafeValue Internal.SomeSEXP
   unprotect = return . Unsafe.mkUnsafe

instance Unprotect a => Unprotect (UnsafeValue a) where
   type UnprotectElt (UnsafeValue a)        = UnprotectElt a
   unprotect x  = Unsafe.unsafeUseValue x unprotect

instance Unprotect (SEXP s a) where
   type UnprotectElt (SEXP s a)          = UnsafeValue (Internal.SEXP a)
   unprotect = return . Unsafe.mkUnsafe . unSEXP

instance Unprotect (SomeSEXP s) where
   type UnprotectElt (SomeSEXP s)       = UnsafeValue Internal.SomeSEXP
   unprotect = return . Unsafe.mkUnsafe . (\(SomeSEXP z) -> Internal.SomeSEXP (unSEXP z))


---------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------

instance Protect s ()
instance Unprotect ()

instance Protect s Int32
instance Unprotect Int32

instance Protect s Double
instance Unprotect Double

instance Protect s Bool
instance Unprotect Bool

instance Protect s a => Protect s [a] where
   type ProtectElt s [a] = [ProtectElt s a]
   protect = mapM protect

instance Unprotect a => Unprotect [a] where
   type UnprotectElt [a] = [UnprotectElt a]
   unprotect = mapM unprotect

