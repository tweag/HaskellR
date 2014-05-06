-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Prelude.Interactive
  ( module H.Prelude
  , printQuote
  )
  where

import H.Prelude hiding (runR)
import qualified H.Prelude as H

instance MonadR IO where
  io = unsafeRunInRThread

-- | A form of the 'print' function that that is more convenient in an
-- interactive session.
printQuote :: (MonadR m, H.Show a) => m a -> m ()
printQuote = (>>= H.print)
