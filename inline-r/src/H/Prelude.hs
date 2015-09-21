-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# Language GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language ViewPatterns #-}

module H.Prelude
  ( module Language.R.Instance
  , module Control.Monad.R.Class
  , module Foreign.R.Error
  -- * Language.R functions
  , module Language.R
  , module Language.R.Event
  , module Language.R.Literal
  -- * Globals
  , module Language.R.Globals
  , Show(..)
  , show
  ) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Foreign.R as R
import           Foreign.R (SEXP, SomeSEXP(..))
import           Language.R.HExp
import           Language.R.Internal (r1)
import qualified Data.Vector.SEXP as Vector

-- Reexported modules.
import           Language.R
import           Language.R.Event (refresh)
import           Language.R.Globals
import           Language.R.Literal
import           Language.R.Instance
import           Foreign.R.Error

import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import           Data.Text.Lazy (Text)

import           Control.Monad ((>=>))
import           Foreign.C (withCString)
import           System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (Show(..), print)

class Show a where
  -- | Equivalent of R's @deparse()@.
  show :: a -> Text

  -- | Make this a class method to allow matching R's @print()@ behaviour, whose
  -- output is subtly different from @deparse()@.
  print :: MonadR m => a -> m ()
  print = io . Text.putStrLn . show

instance Show (SEXP s a) where
  show s =
      unsafePerformIO $
      withCString "quote" $ R.install >=> \quote ->
        R.lang2 quote (R.release s) >>= r1 "deparse" >>= \(SomeSEXP slang) ->
          return .
          Text.Lazy.fromChunks .
          map (Text.pack . Vector.toString . vector) .
          Vector.toList .
          vector $
          (R.unsafeCoerce (R.release slang) :: SEXP V 'R.String)

  print = io . R.printValue

instance Show (R.SomeSEXP s) where
  show s = R.unSomeSEXP s show
  print s = R.unSomeSEXP s print
