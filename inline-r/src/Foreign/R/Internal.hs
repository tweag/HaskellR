-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions which depend on
-- computing offsets of C struct field.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--skip-module=False #-}
module Foreign.R.Internal where

import Control.Memory.Region
import Foreign.R.Type
import Foreign.R.Context (SEXP0(..))

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Singletons (fromSing)
import Foreign (Ptr, Storable(..))
import Foreign.C
import Prelude hiding (asTypeOf, length)


--------------------------------------------------------------------------------
-- R data structures                                                          --
--------------------------------------------------------------------------------

-- | The basic type of all R expressions, classified by the
-- memory region in which it has been allocated.
newtype SEXP s = SEXP { unSEXP :: SEXP0 }
  deriving ( Eq
           , Ord
           , Storable
           )

instance Show (SEXP s) where
  show (SEXP ptr) = show ptr

instance NFData (SEXP s) where
  rnf = (`seq` ())

-- | Add a type index to the pointer.
sexp :: SEXP0 -> SEXP s
sexp = SEXP

-- | Remove the type index from the pointer.
unsexp :: SEXP s -> SEXP0
unsexp = unSEXP

-- LERegion is used only because LH is confused by (<=)
type LERegion t s = t <= s

{-@ release :: LERegion t s => a:SEXP s -> TSEXP t (typeOf a) @-}
-- | Release object into another region. Releasing is safe so long as the target
-- region is "smaller" than the source region, in the sense of
-- '(Control.Memory.Region.<=)'.
release :: (t <= s) => SEXP s -> SEXP t
release = unsafeRelease

{-@ assume unsafeRelease :: a:SEXP s -> TSEXP r (typeOf a) @-}
unsafeRelease :: SEXP s -> SEXP r
unsafeRelease = sexp . unsexp

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . cIntConv

cUIntFromSingEnum :: SSEXPTYPE a -> CUInt
cUIntFromSingEnum = cIntConv . fromEnum . fromSing

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = cIntConv . fromEnum

{-@ opaque-reflect typeOf @-}
-- | Return the \"type\" tag (aka the form tag) of the given 'SEXP'. This
-- function is pure because the type of an object does not normally change over
-- the lifetime of the object.
typeOf :: SEXP s -> SEXPTYPE
typeOf s = unsafeInlineIO $ cIntToEnum <$> cTYPEOF (unsexp s)

{-@ type TSEXP s T = {v:SEXP s | typeOf v == T} @-}

{-@ assume checkSEXPTYPE :: t:SEXPTYPE -> SEXP s -> TSEXP s t @-}
{-@ ignore checkSEXPTYPE @-}
checkSEXPTYPE :: SEXPTYPE -> SEXP s -> SEXP s
checkSEXPTYPE ty s
  | ty == typeOf s = s
  | otherwise =
    error $ "checkType: Dynamic type cast failed. Expected: " ++ show ty ++
            ". Actual: " ++ show (typeOf s) ++ "."

foreign import ccall unsafe "TYPEOF" cTYPEOF :: SEXP0 -> IO CInt

--------------------------------------------------------------------------------
-- Global variables                                                           --
--------------------------------------------------------------------------------

foreign import ccall "&R_Interactive" isRInteractive :: Ptr CInt

{-@ nilValue :: Ptr (TSEXP G Nil) @-}
-- | Global nil value. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_NilValue" nilValue  :: Ptr (SEXP G)

{-@ unboundValue :: Ptr (TSEXP G Symbol) @-}
-- | Unbound marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_UnboundValue" unboundValue :: Ptr (SEXP G)

{-@ missingArg :: Ptr (TSEXP G Symbol) @-}
-- | Missing argument marker. Constant throughout the lifetime of the R instance.
foreign import ccall "&R_MissingArg" missingArg :: Ptr (SEXP G)

{-@ baseEnv :: Ptr (TSEXP G Env) @-}
-- | The base environment.
foreign import ccall "&R_BaseEnv" baseEnv :: Ptr (SEXP G)

{-@ emptyEnv :: Ptr (TSEXP G Env) @-}
-- | The empty environment.
foreign import ccall "&R_EmptyEnv" emptyEnv :: Ptr (SEXP G)

{-@ globalEnv :: Ptr (TSEXP G Env) @-}
-- | Global environment.
foreign import ccall "&R_GlobalEnv" globalEnv :: Ptr (SEXP G)

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
      , infoDebug :: Bool        -- ^ Debug marker.
      } deriving ( Show )

-- | Extract the header from the given 'SEXP'.
peekInfo :: SEXP s -> IO SEXPInfo
peekInfo ts =
    SEXPInfo
      <$> (toEnum.fromIntegral <$> cTYPEOF s)
      <*> ((/=0)              <$> cOBJECT s)
      <*> (fromIntegral       <$> cNAMED s)
      <*> (fromIntegral       <$> cLEVELS s)
      <*> ((/=0)              <$> cRDEBUG s)
  where
    s = unsexp ts

-- These accessors are necessary because hsc2hs cannot determine the offset of
-- C struct bit-fields. https://gitlab.haskell.org/ghc/ghc/-/issues/12149
foreign import ccall unsafe "OBJECT" cOBJECT :: SEXP0 -> IO CInt
foreign import ccall unsafe "NAMED" cNAMED :: SEXP0 -> IO CInt
foreign import ccall unsafe "LEVELS" cLEVELS :: SEXP0 -> IO CInt
foreign import ccall unsafe "RDEBUG" cRDEBUG :: SEXP0 -> IO CInt

-------------------------------------------------------------------------------
-- Attribute header                                                          --
-------------------------------------------------------------------------------

-- | Check if object is an S4 object.
--
-- This is a function call so it will be more precise than using 'typeOf'.
isS4 :: SEXP s -> Bool
isS4 s = (>0) $ cisS4 (unsexp s)

{-@ assume getAttributes :: SEXP s -> IO (TSEXP s List) @-}
-- | Get the attribute list from the given object.
getAttributes :: SEXP s -> IO (SEXP s)
getAttributes s = sexp <$> cAttrib (unsexp s)

{-@ assume getAttribute :: SEXP s -> TSEXP s2 Char -> SEXP s @-}
-- | Get attribute with the given name.
getAttribute :: SEXP s  -- ^ Value
             -> SEXP s2 -- ^ Attribute name
             -> SEXP s
getAttribute a b = sexp $ cgetAttrib (unsexp a) (unsexp b)


{-@ assume setAttributes :: SEXP s -> TSEXP s List -> IO () @-}
-- | Set the attribute list.
setAttributes :: SEXP s -> SEXP s -> IO ()
setAttributes s v = csetAttrib (unsexp s) (unsexp v)

foreign import ccall unsafe "Rinternals.h ATTRIB" cAttrib :: SEXP0 -> IO SEXP0
foreign import ccall unsafe "Rinternals.h SET_ATTRIB" csetAttrib :: SEXP0 -> SEXP0 -> IO ()
foreign import ccall unsafe "Rinternals.h Rf_getAttrib" cgetAttrib :: SEXP0 -> SEXP0 -> SEXP0
foreign import ccall unsafe "Rinternals.h Rf_isS4" cisS4 :: SEXP0 -> Int
