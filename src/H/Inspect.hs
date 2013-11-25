-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- This module is intended to be imported qualified.
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module H.Inspect
  ( inspect )
  where

import H.HExp
import qualified H.Prelude as H
import qualified Foreign.R as R
import qualified Data.Vector.SEXP as Vector
import System.IO.Unsafe ( unsafePerformIO )

-- | Return json code for inspected data
inspect :: R.SEXP a -> String
inspect = go
  where
    ub = R.unsexp H.unboundValue
    nil = R.unsexp H.nilValue
    miss = R.unsexp H.missingArg
    info = unsafePerformIO . R.peekInfo
    go :: R.SEXP a -> String
    go x | R.unsexp x == ub = "\"UnboundValue\""
         | R.unsexp x == nil = "\"NilValue\""
         | R.unsexp x == miss = "\"MissingArg\""
    go (hexp -> Nil) = "null"
    go h@(hexp -> Lang x y) = concat ["{\"Lang\":{\"function\":"
                                   , go x
                                   , ", \"params\":"
                                   , go y
                                   , ", \"info\":\""++show (info h)++"\""
                                   , "}}"]
    go h@(hexp -> Symbol x y z) = concat [ "{\"Symbol\":{\"name\":"
                                       , (go x)
                                       , ", \"value\":"
                                       , if R.unsexp y== R.unsexp h then "\"loop\"" else go y
                                       , ", \"internal\":"
                                       , (maybe "null" go z )
                                       , ", \"info\":\""++show (info h)++"\""
                                       , "}}"]
    go (hexp -> Special i) = "\"Special ("++show i++")\""
    go (hexp -> Char v) = concat [ "{\"Char\": \""++ Vector.toString v++"\"}"]
    go h@(hexp -> List x y z) = concat [ "{\"List\":{"
                                     , "\"value\":"++ (go x)
                                     , ",\"next\":" ++ (maybe "null" go y)
                                     , ",\"tag\":" ++ (maybe "null" go z)
                                     , ", \"info\":\""++show (info h)++"\""
                                     , "}}"
                                     ]
    go x = unsafePerformIO $ do
        ty <- R.typeOf x
        return $ "\""++show ty++"\""


