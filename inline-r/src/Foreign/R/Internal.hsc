-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions which depend on
-- computing offsets of C struct field. We use hsc2hs for this purpose to
-- sidestep issues in c2hs. https://github.com/haskell/c2hs/issues/168
--

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
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
import {-# SOURCE #-} Language.R.HExp (HExp)
import Foreign.R.Type
import Foreign.R.Type as R

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Singletons (fromSing)
#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
#endif
import Foreign (Ptr, castPtr, plusPtr, Storable(..))
import Foreign.C
import Prelude hiding (asTypeOf, length)

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include "missing_r.h"


--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

data {-# CTYPE  "SEXPREC" #-} SEXPREC

-- | The basic type of all R expressions, classified by the form of the
-- expression, and the memory region in which it has been allocated.
newtype SEXP s (a :: SEXPTYPE) = SEXP { unSEXP :: Ptr (HExp s a) }
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

type SEXP0 = Ptr SEXPREC

-- | Add a type index to the pointer.
sexp :: SEXP0 -> SEXP s a
sexp = SEXP . castPtr

-- | Remove the type index from the pointer.
unsexp :: SEXP s a -> SEXP0
unsexp = castPtr . unSEXP

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

--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | Return the \"type\" tag (aka the form tag) of the given 'SEXP'. This
-- function is pure because the type of an object does not normally change over
-- the lifetime of the object.
typeOf :: SEXP s a -> SEXPTYPE
typeOf s = unsafeInlineIO $ cIntToEnum <$> cTYPEOF (unsexp s)

foreign import capi unsafe "TYPEOF" cTYPEOF :: SEXP0 -> IO CInt

-- | Set CAR field of object, when object is viewed as a cons cell.
setCar :: SEXP s a -> SEXP s b -> IO ()
setCar s s' = #{poke SEXPREC, u.listsxp.carval} (unsexp s) (castPtr $ unsexp s')

-- | Set CDR field of object, when object is viewed as a cons cell.
setCdr :: SEXP s a -> SEXP s b -> IO ()
setCdr s s' = #{poke SEXPREC, u.listsxp.cdrval} (unsexp s) (castPtr $ unsexp s')

-- | Set TAG field of object, when object is viewed as a cons cell.
setTag :: SEXP s a -> SEXP s b -> IO ()
setTag s s' = #{poke SEXPREC, u.listsxp.tagval} (unsexp s) (castPtr $ unsexp s')

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
unsafeCoerce = sexp . castPtr . unsexp

--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Length of the vector.
length :: R.IsVector a => SEXP s a -> IO Int
length s = fromIntegral <$>
             (#{peek VECTOR_SEXPREC, vecsxp.length} (unsexp s) :: IO CInt)

-- | Extract the data pointer from a vector.
unsafeSEXPToVectorPtr :: SEXP s a -> Ptr ()
unsafeSEXPToVectorPtr s = (unsexp s) `plusPtr` #{size SEXPREC_ALIGN}

-- | Inverse of 'vectorPtr'.
unsafeVectorPtrToSEXP :: Ptr a -> SomeSEXP s
unsafeVectorPtrToSEXP s = SomeSEXP $ sexp $ s `plusPtr` (- #{size SEXPREC_ALIGN})

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

-- | Class symbol.
foreign import ccall "&R_ClassSymbol" classSymbol :: Ptr (SEXP G R.Symbol)

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
      , infoGcGen :: Int         -- ^ GC Generation.
      , infoGcCls :: Int         -- ^ GC Class of node.
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
      <*> (fromIntegral       <$> cGCGEN s)
      <*> (fromIntegral       <$> cGCCLS s)
  where
    s = unsexp ts

-- These accessors are necessary because hsc2hs cannot determine the offset of
-- C struct bit-fields. https://ghc.haskell.org/trac/ghc/ticket/12149
foreign import capi unsafe "OBJECT" cOBJECT :: SEXP0 -> IO CInt
foreign import capi unsafe "NAMED" cNAMED :: SEXP0 -> IO CInt
foreign import capi unsafe "LEVELS" cLEVELS :: SEXP0 -> IO CInt
foreign import capi unsafe "MARK" cMARK :: SEXP0 -> IO CInt
foreign import capi unsafe "RDEBUG" cRDEBUG :: SEXP0 -> IO CInt
foreign import capi unsafe "RTRACE" cRTRACE :: SEXP0 -> IO CInt
foreign import capi unsafe "RSTEP" cRSTEP :: SEXP0 -> IO CInt
foreign import capi unsafe "missing_r.h GCGEN" cGCGEN :: SEXP0 -> IO CInt
foreign import capi unsafe "missing_r.h GCCLS" cGCCLS :: SEXP0 -> IO CInt

-- | Write a new header.
pokeInfo :: SEXP s a -> SEXPInfo -> IO ()
pokeInfo (unsexp -> s) i = do
    cSET_TYPEOF s (fromIntegral.fromEnum $ infoType i)
    cSET_OBJECT s (if infoObj  i then 1 else 0)
    cSET_NAMED s (fromIntegral $ infoNamed i)
    cSETLEVELS s (fromIntegral $ infoGp i)
    cSET_MARK s (if infoMark i  then 1 else 0)
    cSET_RDEBUG s (if infoDebug i then 1 else 0)
    cSET_RTRACE s (if infoTrace i then 1 else 0)
    cSET_RSTEP s (if infoSpare i then 1 else 0)
    cSET_GCGEN s (fromIntegral $ infoGcGen i)
    cSET_GCCLS s (fromIntegral $ infoGcCls i)

foreign import capi unsafe "SET_TYPEOF" cSET_TYPEOF :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_OBJECT" cSET_OBJECT :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_NAMED" cSET_NAMED :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SETLEVELS" cSETLEVELS :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_MARK" cSET_MARK :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_RDEBUG" cSET_RDEBUG :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_RTRACE" cSET_RTRACE :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "SET_RSTEP" cSET_RSTEP :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "missing_r.h SET_GCGEN" cSET_GCGEN :: SEXP0 -> CInt -> IO ()
foreign import capi unsafe "missing_r.h SET_GCCLS" cSET_GCCLS :: SEXP0 -> CInt -> IO ()

-- | Set the GC mark.
mark :: Bool -> SEXP s a -> IO ()
mark b ts = cSET_MARK (unsexp ts) (if b then 1 else 0)

named :: Int -> SEXP s a -> IO ()
named v ts = cSET_NAMED (unsexp ts) (fromIntegral v)

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

-- | Get the attribute list from the given object.
getAttribute :: SEXP s a -> IO (SEXP s b)
getAttribute s = sexp <$> cATTRIB (unsexp s)

-- | Set the attribute list.
setAttribute :: SEXP s a -> SEXP s b -> IO ()
setAttribute s v = cSET_ATTRIB (unsexp s) (castPtr $ unsexp v)

-- | Set concrete attribute.
setAttrib :: SEXP s1 a -> SEXP s2 R.Symbol -> SEXP s3 b -> IO ()
setAttrib o s v = cSetAttrib (unsexp o) (unsexp s) (unsexp v)

duplicateAttrib :: SEXP s a -> SEXP s2 b -> IO ()
duplicateAttrib to from = cDUPLICATE_ATTRIB (unsexp to) (unsexp from)

foreign import capi unsafe "Rinternals.h ATTRIB" cATTRIB :: SEXP0 -> IO SEXP0
foreign import capi unsafe "Rinternals.h SET_ATTRIB" cSET_ATTRIB :: SEXP0 -> SEXP0 -> IO ()

foreign import capi unsafe "Rinternals.h Rf_setAttrib" cSetAttrib :: SEXP0 -> SEXP0 -> SEXP0 -> IO ()
foreign import capi unsafe "Rinternals.h DUPLICATE_ATTRIB" cDUPLICATE_ATTRIB :: SEXP0 -> SEXP0 -> IO ()
