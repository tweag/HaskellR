{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Parser
  ( tests )
  where

import Control.Applicative
import Data.Int
import H.Prelude
import qualified Foreign.R as R
import Language.R.Matcher as P

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "matcher"
  [ testCase "null" $ ((True @=?) =<<) $ do
       runRegion $ do
         s <- [r| NULL |]
         Right t <- matchOnly (P.null *> pure True <|> pure False) s
         return t
  , testCase "s3: pass non s3" $ ((True @=?) =<<) $ do
     runRegion  $ do
       s <- [r| c(1:10) |]
       Right t <- matchOnly (P.s3 ["matrix"] *> pure False <|> pure True) s
       return t
  , testCase "s3: matches matrix" $ ((True @=?) =<<) $ do
     runRegion  $ do
       s <- [r| x <- matrix(c(1:10)); class(x) <- "shmatrix"; x |]
       Right t <- matchOnly (P.s3 ["shmatrix"] *> pure True <|> pure False) s
       return t
  , testCase "typeOf: reads type" $ ((R.Int @=?) =<<) $ do
     runRegion $ do
       s <- [r| matrix(c(1:10)) |]
       Right t <- matchOnly P.typeOf s
       return t
  , testCase "guardType: proceeds" $ ((True @=?) =<<) $ do
     runRegion $ do
       s <- [r| matrix(c(1:10)) |]
       Right t <- matchOnly (P.guardType R.Int *> pure True <|> pure False) s
       return t
  , testCase "guardType: fails" $ ((True @=?) =<<) $ do
     runRegion $ do
       s <- [r| 1.0 |]
       Right t <- matchOnly (P.guardType R.Int *> pure False <|> pure True ) s
       return t
  , testCase "someAttribute" $ (([2,3::Int32] @=?) =<<) $ do
     runRegion $ do
       s <- [r| matrix(c(1:6), 2,3) |]
       Right t <- matchOnly (P.someAttribute "dim") s
       return (fromSEXP (R.cast SInt t))
  , testCase "someAttribute" $ (([2,3::Int32] @=?) =<<) $ do
     runRegion $ do
       s <- [r| matrix(c(1:6), 2,3) |]
       Right t <- matchOnly (P.attribute SInt "dim") s
       return (fromSEXP t)
  , testCase "getS3Class" $ ((["shmatrix"] @=?) =<<) $ do
     runRegion $ do
       s <- [r| x <- matrix(c(1:10),2,3); class(x) <- "shmatrix"; x |]
       Right t <- matchOnly P.getS3Class s
       return t
  ]
