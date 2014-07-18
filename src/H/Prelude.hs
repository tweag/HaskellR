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
  , module Language.R.Literal.Unsafe
  , module Language.R.Literal
  -- * Globals
  , module Language.R.Globals
  , Show(..)
  , show
  , withProtected
  -- * Type convertion helpers.
  , toBool
  ) where


import           H.Internal.Prelude
import qualified Foreign.R.Internal as R
import qualified Foreign.R as FR
import Language.R.HExp.Unsafe
import qualified Data.Vector.SEXP as Vector

-- Reexported modules.
import           Control.Monad.R
import           Language.R.Globals
import           Language.R.Literal.Unsafe
import           Language.R.Literal
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


instance Show (SEXP a) where
  showIO s = Language.R.withProtected (return s) $ \_ ->
           withCString "quote" $ R.install >=> \quote ->
           R.lang2 quote s >>= r1 "deparse" >>= \(SomeSEXP slang) ->
           return .
           Text.Lazy.fromChunks .
           map (Text.pack . Vector.toString . vector) .
           Vector.toList .
           vector $
           (R.unsafeCoerce slang :: SEXP R.String)

  print e = io $ Language.R.withProtected (return e) R.printValue

instance Show R.SomeSEXP where
  showIO s = R.unSomeSEXP s showIO
  print s = R.unSomeSEXP s print

instance Show (FR.SEXP s a) where
  showIO (FR.SEXP s) = showIO s
  print  (FR.SEXP s) = print s

instance Show (FR.SomeSEXP s) where
  showIO (FR.SomeSEXP s) = showIO s
  print  (FR.SomeSEXP s) = print s

withProtected :: (MonadR m, MonadCatch m, MonadMask m) => m (SEXP a) -> ((SEXP a) -> m b) -> m b
withProtected accure =
    bracket (accure >>= \x -> io $ R.protect x >> return x)
            (const (io $ R.unprotect 1))

-- | Convert Logical value into boolean.
toBool :: SomeSEXP -> Bool
toBool (SomeSEXP z) = case hexp z of
  Logical vt -> vt `unsafeIndex` 0 == R.True
  _          -> False
