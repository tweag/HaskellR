-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Prelude.Interactive
  ( module H.Prelude
  , p
  , printQuote
  )
  where

import H.Prelude hiding (withEmbeddedR)
import qualified H.Prelude as H

instance MonadR IO where
  io = id
  unsafeToIO = id

-- | A form of the 'print' function that is more convenient in an
-- interactive session.
p :: (MonadR m, H.Show a) => m a -> m ()
p = (>>= H.print)

-- | A form of the 'print' function that that is more convenient in an
-- interactive session.
{-# DEPRECATED printQuote "Use 'p' instead." #-}
printQuote :: (MonadR m, H.Show a) => m a -> m ()
printQuote = p
