-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE CPP #-}
{-# Language GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language ViewPatterns #-}

module H.Prelude
  ( module Control.Monad.R
  , module Foreign.R.Error
  -- * Language.R functions
  , module Language.R
  -- * Literals
  , module Language.R.Literal
  -- * Globals
  , module Language.R.Globals
  -- * HExp
  , Show(..)
  , show
  -- * Type convertion helpers.
  , toBool
  ) where


import qualified Foreign.R.Internal as Internal
import           Foreign.R (SEXP(..), SomeSEXP(..))
import qualified Language.R.HExp.Unsafe as Unsafe
import qualified Data.Vector.SEXP as Vector
import           Control.Monad.R.Unsafe()

-- Reexported modules.
import           Control.Monad.R
import           Language.R.Globals
import           Language.R.Literal
import           Language.R hiding ( withProtected )
import qualified Language.R ( withProtected )
import Foreign.R.Error

import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy (Text)
import Data.Vector.SEXP (unsafeIndex)

import Control.Monad ((>=>))
import Foreign.C (withCString)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (Show(..), print)

class Show a where
  -- | Equivalent of R's @deparse()@.
  showIO :: a -> IO Text

  -- | Make this a class method to allow matching R's @print()@ behaviour, whose
  -- output is subtly different from @deparse()@.
  print :: MonadR m => a -> m ()
  print = io . (showIO >=> Text.putStrLn)

-- | Pure version of 'showIO'.
show :: Show a => a -> Text
show = unsafePerformIO . showIO


instance Show (Internal.SEXP a) where
  showIO s = Language.R.withProtected (return s) $ \_ ->
           withCString "quote" $ Internal.install >=> \quote ->
           Internal.lang2 quote s >>= r1 "deparse" >>= \(Internal.SomeSEXP slang) ->
           return .
           Text.Lazy.fromChunks .
           map (Text.pack . Vector.toString . Unsafe.vector) .
           Vector.toList .
           Unsafe.vector $
           (Internal.unsafeCoerce slang :: Internal.SEXP Internal.String)

  print e = io $ Language.R.withProtected (return e) Internal.printValue

instance Show Internal.SomeSEXP where
  showIO s = Internal.unSomeSEXP s showIO
  print s = Internal.unSomeSEXP s print


instance Show (SEXP s a) where
  showIO (SEXP s) = showIO s
  print  (SEXP s) = print s

instance Show (SomeSEXP s) where
  showIO (SomeSEXP s) = showIO s
  print  (SomeSEXP s) = print s


instance Show a => Show (UnsafeValue a) where
  showIO = flip unsafeUseValue showIO
  print  = flip unsafeUseValue print


-- | Convert Logical value into boolean.
toBool :: Internal.SomeSEXP -> Bool
toBool (Internal.SomeSEXP z) = case Unsafe.hexp z of
  Unsafe.Logical vt -> vt `unsafeIndex` 0 == Internal.True
  _          -> False
