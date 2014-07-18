-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This class is not meant to be imported in any other circumstance than in
-- a GHCi session.

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module H.Prelude.Interactive
  ( module H.Prelude
  , printQuote
  -- * Region support
  , performR
  , closeRegion
  )
  where

import H.Prelude hiding (runR)
import qualified H.Prelude as H
import Foreign.R.Internal as R
import Foreign.R as FR
import Control.Monad.R.Unsafe

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader

instance MonadR IO where
  io = unsafeRunInRThread

stackSize :: IORef Int
stackSize = unsafePerformIO $ newIORef 0
{-# NOINLINE stackSize #-}

-- | Run R computation inside interpreter. All protected variables will be stored
-- in the toplevel block, that can be closed by calling 'closeRegion'
performR :: H.Unprotect a => (R s a) -> IO (H.UnprotectElt a)
performR r = runReaderT (unR (r >>= FR.unprotect)) stackSize

-- | Mark all used variables as free.
-- This method will not trigger garbage collection until any allocation happed,
-- so following code should be safe:
--
-- > a <- performR $ [r| 1 |]
-- > a <- performR $ [r| 2 |]
-- > closeRegion
-- > performR $ protect b
closeRegion :: IO ()
closeRegion = R.unprotect =<< readIORef stackSize


-- | A form of the 'print' function that that is more convenient in an
-- interactive session.
printQuote :: (MonadR m, H.Show a) => m a -> m ()
printQuote = (>>= H.print)
