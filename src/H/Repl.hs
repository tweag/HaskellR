-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
module H.Repl
  ( replCommand
  ) where

#ifdef CABAL_WINDOWS
import H.Repl.Windows
#else
import H.Repl.Unix
#endif
