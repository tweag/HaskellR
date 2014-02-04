-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

module H.Prelude.Interactive
  ( module H.Prelude
  , module Language.R.Runtime.QQ
  , printQuote
  )
  where

import H.Prelude hiding (runR)
import Language.R.Runtime.QQ
import qualified H.Prelude as H

-- | A form of the 'print' function that that is more convenient in an
-- interactive session.
printQuote :: H.Show a => IO a -> IO ()
printQuote = (>>= H.print)
