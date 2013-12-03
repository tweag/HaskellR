-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.R.Error
  ( RError(..)
  , throwR
  , throwRMessage
  ) where

import Foreign.R as R

import Control.Exception
import Control.Monad ( (>=>) )
import Data.Typeable
import Foreign ( peek )
import Foreign.C.String ( withCString, peekCString )

data RError = RError 
      { rerrorMsg  :: String
      } deriving ( Typeable )

instance Show RError where
  show (RError s) = "RError: "++s

instance Exception RError

-- | Read last error message.
getErrMsg :: R.SEXP R.Env -> IO String
getErrMsg e = do
  f <- withCString "geterrmessage" (R.install >=> R.lang1)
  peekCString =<< char =<< peek =<< string =<< R.eval f e

-- | Throw R exception.
throwR :: R.SEXP R.Env  -- Environment to search error.
       -> IO a
throwR x = getErrMsg x >>= throwIO . RError 

-- | Throw R exception with specified message.
throwRMessage :: String -> IO a
throwRMessage = throwIO . RError
