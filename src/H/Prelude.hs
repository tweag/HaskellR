-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE CPP #-}
{-# Language GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language ViewPatterns #-}

module H.Prelude
  ( module Language.R.Instance
  , module Control.Monad.R.Class
  , module Foreign.R.Error
  -- * Language.R functions
  , module Language.R
  -- * Literals
  , module Language.R.Literal
  -- * Globals
  , module Language.R.Globals
  , Show(..)
  , show
  , withProtected
  -- * Type convertion helpers.
  , toBool
  ) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import           H.Internal.Prelude
import qualified Foreign.R as R
import Language.R.HExp
import qualified Data.Vector.SEXP as Vector

-- Reexported modules.
import           Language.R.Globals
import           Language.R.Literal
import Language.R.Instance
import           Language.R hiding ( withProtected )
import qualified Language.R ( withProtected )
import Foreign.R.Error

import Control.Monad.Catch
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy (Text)
import Data.Vector.Generic (unsafeIndex)

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


instance Show (SEXP s a) where
  showIO s = Language.R.withProtected (return (R.release s)) $ \_ ->
           withCString "quote" $ R.install >=> \quote ->
           R.lang2 quote (R.release s) >>= r1 "deparse" >>= \(SomeSEXP slang) ->
           return .
           Text.Lazy.fromChunks .
           map (Text.pack . Vector.toString . vector) .
           Vector.toList .
           vector $
           (R.unsafeCoerce (R.release slang) :: SEXP V R.String)

  print e = io $ Language.R.withProtected (return (R.release e)) R.printValue

instance Show (R.SomeSEXP s) where
  showIO s = R.unSomeSEXP s showIO
  print s = R.unSomeSEXP s print

withProtected :: (MonadR m, MonadCatch m, MonadMask m) => m (SEXP s a) -> ((SEXP s a) -> m b) -> m b
withProtected accure =
    bracket (accure >>= \x -> io $ R.protect x >> return x)
            (const (io $ R.unprotect 1))

-- | Convert Logical value into boolean.
toBool :: SomeSEXP s -> Bool
toBool (SomeSEXP z) = case hexp z of
  Logical vt -> vt `unsafeIndex` 0 == R.True
  _          -> False
