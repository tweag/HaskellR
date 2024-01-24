-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Prelude.Interactive
  ( module H.Prelude
  , PrintR(..)
  , p
  , printQuote
  )
  where

import qualified Foreign.R as R
import H.Prelude hiding (withEmbeddedR)

instance MonadR IO where
  io = id
  data ExecContext IO = ExecContext
  getExecContext = return ExecContext
  unsafeRunWithExecContext = const

class PrintR a where
  printR :: MonadR m => a -> m ()

instance PrintR (SEXP s) where
  printR = io . R.printValue

-- | A form of the 'printR' function that is more convenient in an interactive
-- session.
p :: (MonadR m, PrintR a) => m a -> m ()
p = (>>= printR)

-- | A form of the 'printR' function that is more convenient in an interactive
-- session.
{-# DEPRECATED printQuote "Use 'p' instead." #-}
printQuote :: (MonadR m, PrintR a) => m a -> m ()
printQuote = p
