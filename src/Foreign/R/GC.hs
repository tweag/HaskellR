-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module provides helper functions and structures for safe memory
-- management when communicating with R.
--
-- 'RVal' is a wrapper around 'SEXP' preventing the 'SEXP' from being garbage
-- collected by R until the Haskell garbage collector signals that it is safe to
-- do so.
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
module Foreign.R.GC
  ( -- * RVal
    RVal
  , newRVal
  , withRVal
  , AsSEXP(..)
  ) where

import Foreign.R.Internal (SomeSEXP(..))
import Foreign.R.Runner (getPostToCurrentRThread)
import qualified Foreign.R.Type as R
import qualified Foreign.R.Internal as R

import Control.Exception ( bracket )
import Foreign ( ForeignPtr, touchForeignPtr, castPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Concurrent ( newForeignPtr )


-- | RVal is a wrapper value that protects the 'R.SEXP' object
-- from beign garbage collected. In order to perform it objects
-- are put into protection object.
--
-- R Values are stored in protection objec that is a single linked
-- list that contains references to all protected objects, this
-- means that creation and deallocation of such objects can take
-- up to @O(N)@ worst case time, where @N@ is a number of objects
-- in protection lists both from @R@ and @Haskell@
--
-- Thus it can be used for any objectthat is isomorphic to 'R.SEXP'.
newtype RVal (a::R.SEXPTYPE) = RVal (ForeignPtr R.SEXPREC)

-- | Create a new 'RVal' object.
newRVal :: AsSEXP a b => a -> IO (RVal b)
newRVal (asSEXP -> s) = do
    R.preserveObject s
    post <- getPostToCurrentRThread
    fmap RVal $ newForeignPtr (R.unsexp s) (post $ R.releaseObject (castPtr $ R.unsexp s))

-- | Work with protected object.
withRVal :: RVal a -> (R.SEXP a -> IO b) -> IO b
withRVal (RVal s) =
  bracket (return . R.sexp . unsafeForeignPtrToPtr $ s)
          (const $ touchForeignPtr s)

-- | Value that can be converted to the SEXP without any side effects.
-- Basically any newtype or data wrappers can be an instance of this
-- type class.
class AsSEXP a b | a -> b  where
  asSEXP :: a -> R.SEXP b

instance AsSEXP (R.SEXP a) a where
  asSEXP = id

instance AsSEXP R.SomeSEXP R.Any where
  asSEXP (SomeSEXP z) = R.unsafeCoerce z
