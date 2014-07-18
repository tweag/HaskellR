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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.R
  ( R
    -- * Internal R structures
  , SEXPTYPE(..)
  , Internal.SEXPInfo
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
import           Control.Monad.R.Class
import           Foreign.R.Internal (SEXPTYPE(..), SSEXPTYPE)
import           Foreign.R.Internal ( CEType )
import qualified Foreign.R.Internal as Internal
import           Foreign.R.GC

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
import Data.IORef

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

instance AsSEXP (SEXP s a) a where
  asSEXP (SEXP x) = x

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

instance AsSEXP (SomeSEXP s) Internal.Any where
  asSEXP (SomeSEXP (SEXP x)) = Internal.unsafeCoerce x

-- | Deconstruct a 'SomeSEXP'. Takes a continuation since otherwise the
-- existentially quantified variable hidden inside 'SomeSEXP' would escape.
unSomeSEXP :: SomeSEXP s -> (forall a. SEXP s a -> r) -> r
unSomeSEXP (SomeSEXP s) k = k s

-- | Foreign functions are represented in R as external pointers. We call these
-- "callbacks", because they will typically be Haskell functions passed as
-- arguments to higher-order R functions.
type Callback s = SEXP s ExtPtr

--------------------------------------------------------------------------------
-- Monad R Class                                                              --
--------------------------------------------------------------------------------
instance MonadR (R s) where
  type RRegion (R s) = s
  io m = Unsafe.R $ ReaderT $ \_ -> m
  increment = Unsafe.R $ ReaderT $ flip modifyIORef' succ

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
mkString :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.String))
         => CString -> m (SEXP s Internal.String)
mkString = liftProtect . Internal.mkString

-- | Initialize a new character vector (aka a string).
mkChar :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.Char))
       => CString -> m (SEXP s Internal.Char)
mkChar = liftProtect . Internal.mkChar

-- | Create Character value with specified encoding
mkCharCE :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.Char))
         => CString -> CEType -> m (SEXP s Internal.Char)
mkCharCE s t = liftProtect $ Internal.mkCharCE s t

-- | Probe the symbol table
install :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.Symbol))
        => CString -> m (SEXP s Internal.Symbol)
install = liftProtect . Internal.install

-- | Allocate a 'SEXP'.
allocSEXP :: (MonadR m, s ~ RRegion m, Protect m (SEXP s a))
          => SSEXPTYPE a -> m (SEXP s a)
allocSEXP = liftProtect . Internal.allocSEXP

-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.List))
          => Int -> m (SEXP s Internal.List)
allocList = liftProtect . Internal.allocList

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: (MonadR m, s ~ RRegion m, Protect m (SEXP s Internal.List))
     => SEXP s a -> SEXP s b -> m (SEXP s Internal.List)
cons a b = liftProtect $ Internal.cons (unSEXP a) (unSEXP b)

mkWeakRef :: (MonadR m, s ~ RRegion m, Protect m (R s (SEXP s Internal.WeakRef)))
          => SEXP s a -> SEXP s b -> SEXP s c -> Bool -> m (SEXP s Internal.WeakRef)
mkWeakRef a b c d = liftProtect $ Internal.mkWeakRef (unSEXP a) (unSEXP b) (unSEXP c) d

--
-- | Print a string representation of a 'SEXP' on the console.
printValue :: (MonadR m, s ~ RRegion m) => SEXP s a -> m ()
printValue = io . Internal.printValue . unSEXP

-- | Function for processing GUI and other events in the internal event loop.
processEvents :: MonadR m => m ()
processEvents = io Internal.processEvents

#ifdef H_ARCH_UNIX
processGUIEventsUnix :: MonadR m => Ptr (Ptr ()) -> m ()
processGUIEventsUnix = io . Internal.processGUIEventsUnix
#endif

-- | Invoke an R garbage collector sweep.
gc :: MonadR m => m ()
gc = io Internal.gc

--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
eval :: (MonadR m, Protect m (SomeSEXP s), s ~ RRegion m)
     => SEXP s a -> SEXP s Env -> m (SomeSEXP s)
eval a b = liftProtect $ Internal.eval (unSEXP a) (unSEXP b)

-- | Try to evaluate expression.
tryEval :: (MonadR m, Protect m (SomeSEXP s), s ~ RRegion m)
        => SEXP s a -> SEXP s Env -> Ptr CInt -> m (SomeSEXP s)
tryEval a b c = liftProtect $ Internal.tryEval (unSEXP a) (unSEXP b) c

-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: (MonadR m, Protect m (SomeSEXP s), s ~ RRegion m)
              => SEXP s a -> SEXP s Env -> Ptr CInt -> m (SomeSEXP s)
tryEvalSilent a b c = liftProtect $ Internal.tryEvalSilent (unSEXP a) (unSEXP b) c

lang1 :: (MonadR m, Protect m (SEXP s Internal.Lang), s ~ RRegion m)
      => SEXP s a -> m (SEXP s Internal.Lang)
lang1 = liftProtect . Internal.lang1 . unSEXP

lang2 :: (MonadR m, Protect m (SEXP s Internal.Lang), s ~ RRegion m)
      => SEXP s a -> SEXP s b ->  m (SEXP s Internal.Lang)
lang2 a b = liftProtect $ Internal.lang2 (unSEXP a) (unSEXP b)

lang3 :: (MonadR m, Protect m (SEXP s Internal.Lang), s ~ RRegion m)
      => SEXP s a -> SEXP s b -> SEXP s c -> m (SEXP s Internal.Lang)
lang3 a b c = liftProtect $ Internal.lang3 (unSEXP a) (unSEXP b) (unSEXP c)

-- | Find a function by name.
findFun :: (MonadR m, Protect m (SomeSEXP s), s ~ RRegion m)
        => SEXP s a -> SEXP s Env ->  m (SomeSEXP s)
findFun a e = liftProtect $ Internal.findFun (unSEXP a) (unSEXP e)

-- | Find a variable by name.
findVar :: (MonadR m, Protect m (SEXP s Internal.Symbol), s ~ RRegion m)
        => SEXP s a -> SEXP s Env -> m (SEXP s Internal.Symbol)
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
getAttribute :: (MonadR m, Protect m (SEXP s b), RRegion m ~ s)
             => SEXP s a -> m (SEXP s b)
getAttribute = io . fmap SEXP . Internal.getAttribute . unSEXP

-- | Set the attribute list.
setAttribute :: (MonadR m, RRegion m ~ s) => SEXP s a -> SEXP s b -> m ()
setAttribute (SEXP a) (SEXP b) = io $ Internal.setAttribute a b

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

liftProtect :: (MonadR m, Protect m a) => IO a -> m (ProtectElt m a)
liftProtect = io >=> protect

-- | Values that can be protected in a region. Basically this typeclass
-- is created in order to have an overloaded protect function, so it can
-- be called on any value and still to the right thing.
--
-- There is a default values for 'ProtectElt' and 'protect' functions
-- that allowes to add an instances to arbitrary datatypes that shouldn't
-- be protected.
class Monad m => Protect m a where
   -- | A new type that user will have after a protection
   type ProtectElt m a :: *
   type ProtectElt m a = a
   -- | Run a protect action. This method really protects value withing
   -- a block, old value is still safe to use, but you should not do it
   -- for safety reasons.
   protect   :: a -> m (ProtectElt m a)
   default protect :: (Monad m, ProtectElt m a ~ a) => a -> m a
   protect = return
   {-# INLINE protect #-}

instance MonadR m => Protect m (Internal.SEXP a) where
   type ProtectElt m (Internal.SEXP a) = SEXP (RRegion m) a
   protect = fmap SEXP . Unsafe.protect

instance MonadR m => Protect m (Internal.SomeSEXP) where
   type ProtectElt m Internal.SomeSEXP = SomeSEXP (RRegion m)
   protect (Internal.SomeSEXP f) = fmap (SomeSEXP . SEXP) (Unsafe.protect f)

instance Protect m a => Protect m (UnsafeValue a) where
   type ProtectElt m (UnsafeValue a)   = ProtectElt m a
   protect a = Unsafe.unsafeUseValue a protect

instance (RRegion m ~ s, MonadR m) => Protect m (SEXP s a) where
   type ProtectElt m (SEXP s a)        = SEXP s a
   protect = return

instance (RRegion m ~ s, MonadR m) => Protect m (SomeSEXP s) where
   type ProtectElt m (SomeSEXP s)      = SomeSEXP s
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

instance Unprotect (RVal a) where
   unprotect = return

instance Unprotect SEXPTYPE where
   unprotect = return

instance Monad s => Protect s ()
instance Unprotect ()

instance Monad s => Protect s Int32
instance Unprotect Int32

instance Monad s => Protect s Double
instance Unprotect Double

instance Monad s => Protect s Bool
instance Unprotect Bool

instance Protect s a => Protect s [a] where
   type ProtectElt s [a] = [ProtectElt s a]
   protect = mapM protect

instance Unprotect a => Unprotect [a] where
   type UnprotectElt [a] = [UnprotectElt a]
   unprotect = mapM unprotect

