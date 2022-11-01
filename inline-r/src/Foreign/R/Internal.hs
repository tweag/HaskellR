-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions which depend on
-- computing offsets of C struct field. We use hsc2hs for this purpose.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 710
-- We don't use ticks in this module, because they confuse hsc2hs.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif
module Foreign.R.Internal where

import Control.Memory.Region
import Foreign.R.Type
import Foreign.R.Type as R
import Foreign.R.Context (SEXP0(..))

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Singletons (fromSing)
#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
#endif
import Foreign (Ptr, castPtr, Storable(..))
import Foreign.C
import Prelude hiding (asTypeOf, length)


--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

-- | The basic type of all R expressions, classified by the form of the
-- expression, and the memory region in which it has been allocated.
newtype SEXP s (a :: SEXPTYPE) = SEXP { unSEXP :: SEXP0 }
  deriving ( Eq
           , Ord
           , Storable
#if __GLASGOW_HASKELL__ < 710
           , Typeable
#endif
           )

instance Show (SEXP s a) where
  show (SEXP ptr) = show ptr

instance NFData (SEXP s a) where
  rnf = (`seq` ())

-- | Add a type index to the pointer.
sexp :: SEXP0 -> SEXP s a
sexp = SEXP

-- | Remove the type index from the pointer.
unsexp :: SEXP s a -> SEXP0
unsexp = unSEXP

-- | Like 'sexp' but for 'SomeSEXP'.
somesexp :: SEXP0 -> SomeSEXP s
somesexp = SomeSEXP . sexp

-- | Release object into another region. Releasing is safe so long as the target
-- region is "smaller" than the source region, in the sense of
-- '(Control.Memory.Region.<=)'.
release :: (t <= s) => SEXP s a -> SEXP t a
release = unsafeRelease

unsafeRelease :: SEXP s a -> SEXP r a
unsafeRelease = sexp . unsexp

unsafeReleaseSome :: SomeSEXP s -> SomeSEXP g
unsafeReleaseSome (SomeSEXP x) = SomeSEXP (unsafeRelease x)

-- | A 'SEXP' of unknown form.
data SomeSEXP s = forall a. SomeSEXP {-# UNPACK #-} !(SEXP s a)

instance Show (SomeSEXP s) where
  show s = unSomeSEXP s show

instance Storable (SomeSEXP s) where
  sizeOf _ = sizeOf (undefined :: SEXP s a)
  alignment _ = alignment (undefined :: SEXP s a)
  peek ptr = SomeSEXP <$> peek (castPtr ptr)
  poke ptr (SomeSEXP s) = poke (castPtr ptr) s

instance NFData (SomeSEXP s) where
  rnf = (`seq` ())

-- | Deconstruct a 'SomeSEXP'. Takes a continuation since otherwise the
-- existentially quantified variable hidden inside 'SomeSEXP' would escape.
unSomeSEXP :: SomeSEXP s -> (forall a. SEXP s a -> r) -> r
unSomeSEXP (SomeSEXP s) k = k s

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . cIntConv

cUIntFromSingEnum :: SSEXPTYPE a -> CUInt
cUIntFromSingEnum = cIntConv . fromEnum . fromSing

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = cIntConv . fromEnum

-- | Return the \"type\" tag (aka the form tag) of the given 'SEXP'. This
-- function is pure because the type of an object does not normally change over
-- the lifetime of the object.
typeOf :: SEXP s a -> SEXPTYPE
typeOf s = unsafeInlineIO $ cIntToEnum <$> cTYPEOF (unsexp s)

foreign import ccall unsafe "TYPEOF" cTYPEOF :: SEXP0 -> IO CInt

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

unsafeCast :: SEXPTYPE -> SomeSEXP s -> SEXP s b
unsafeCast ty (SomeSEXP s)
  | ty == typeOf s = unsafeCoerce s
  | otherwise =
    error $ "cast: Dynamic type cast failed. Expected: " ++ show ty ++
            ". Actual: " ++ show (typeOf s) ++ "."

-- | Cast the type of a 'SEXP' into another type. This function is partial: at
-- runtime, an error is raised if the source form tag does not match the target
-- form tag.
cast :: SSEXPTYPE a -> SomeSEXP s -> SEXP s a
cast ty s = unsafeCast (fromSing ty) s

-- | Cast form of first argument to that of the second argument.
asTypeOf :: SomeSEXP s -> SEXP s a -> SEXP s a
asTypeOf s s' = typeOf s' `unsafeCast` s

-- | Unsafe coercion from one form to another. This is unsafe, in the sense that
-- using this function improperly could cause code to crash in unpredictable
-- ways. Contrary to 'cast', it has no runtime cost since it does not introduce
-- any dynamic check at runtime.
unsafeCoerce :: SEXP s a -> SEXP s b
unsafeCoerce = sexp . unsexp

--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

foreign import ccall "&R_Interactive" isRInteractive :: Ptr CInt

-- | Global nil value. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_NilValue" nilValue  :: Ptr (SEXP G R.Nil)

-- | Unbound marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_UnboundValue" unboundValue :: Ptr (SEXP G R.Symbol)

-- | Missing argument marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_MissingArg" missingArg :: Ptr (SEXP G R.Symbol)

-- | The base environment.
foreign import ccall "&R_BaseEnv" baseEnv :: Ptr (SEXP G R.Env)

-- | The empty environment.
foreign import ccall "&R_EmptyEnv" emptyEnv :: Ptr (SEXP G R.Env)

-- | Global environment.
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP G R.Env)

-- | Signal handler switch
foreign import ccall "&R_SignalHandlers" signalHandlers :: Ptr CInt

-- | Flag that shows if computation should be interrupted.
foreign import ccall "&R_interrupts_pending" interruptsPending :: Ptr CInt

----------------------------------------------------------------------------------
-- Structure header                                                             --
----------------------------------------------------------------------------------

-- | Info header for the SEXP data structure.
data SEXPInfo = SEXPInfo
      { infoType  :: SEXPTYPE    -- ^ Type of the SEXP.
      , infoObj   :: Bool        -- ^ Is this an object with a class attribute.
      , infoNamed :: Int         -- ^ Control copying information.
      , infoGp    :: Int         -- ^ General purpose data.
      , infoMark  :: Bool        -- ^ Mark object as 'in use' in GC.
      , infoDebug :: Bool        -- ^ Debug marker.
      , infoTrace :: Bool        -- ^ Trace marker.
      , infoSpare :: Bool        -- ^ Alignment (not in use).
      } deriving ( Show )

-- | Extract the header from the given 'SEXP'.
peekInfo :: SEXP s a -> IO SEXPInfo
peekInfo ts =
    SEXPInfo
      <$> (toEnum.fromIntegral <$> cTYPEOF s)
      <*> ((/=0)              <$> cOBJECT s)
      <*> (fromIntegral       <$> cNAMED s)
      <*> (fromIntegral       <$> cLEVELS s)
      <*> ((/=0)              <$> cMARK s)
      <*> ((/=0)              <$> cRDEBUG s)
      <*> ((/=0)              <$> cRTRACE s)
      <*> ((/=0)              <$> cRSTEP s)
  where
    s = unsexp ts

-- These accessors are necessary because hsc2hs cannot determine the offset of
-- C struct bit-fields. https://ghc.haskell.org/trac/ghc/ticket/12149
foreign import ccall unsafe "OBJECT" cOBJECT :: SEXP0 -> IO CInt
foreign import ccall unsafe "NAMED" cNAMED :: SEXP0 -> IO CInt
foreign import ccall unsafe "LEVELS" cLEVELS :: SEXP0 -> IO CInt
foreign import ccall unsafe "MARK" cMARK :: SEXP0 -> IO CInt
foreign import ccall unsafe "RDEBUG" cRDEBUG :: SEXP0 -> IO CInt
foreign import ccall unsafe "RTRACE" cRTRACE :: SEXP0 -> IO CInt
foreign import ccall unsafe "RSTEP" cRSTEP :: SEXP0 -> IO CInt

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

-- | Check if object is an S4 object.
--
-- This is a function call so it will be more precise than using 'typeOf'.
isS4 :: SEXP s ty -> Bool
isS4 s = (>0) $ cisS4 (unsexp s)

-- | Get the attribute list from the given object.
getAttributes :: SEXP s a -> IO (SEXP s b)
getAttributes s = sexp <$> cAttrib (unsexp s)

-- | Get attribute with the given name.
getAttribute :: SEXP s  a -- ^ Value
             -> SEXP s2 b -- ^ Attribute name
             -> SEXP s  c
getAttribute a b = sexp $ cgetAttrib (unsexp a) (unsexp b)


-- | Set the attribute list.
setAttributes :: SEXP s a -> SEXP s b -> IO ()
setAttributes s v = csetAttrib (unsexp s) (unsexp v)

foreign import ccall unsafe "Rinternals.h ATTRIB" cAttrib :: SEXP0 -> IO SEXP0
foreign import ccall unsafe "Rinternals.h SET_ATTRIB" csetAttrib :: SEXP0 -> SEXP0 -> IO ()
foreign import ccall unsafe "Rinternals.h Rf_getAttrib" cgetAttrib :: SEXP0 -> SEXP0 -> SEXP0
foreign import ccall unsafe "Rinternals.h Rf_isS4" cisS4 :: SEXP0 -> Int
