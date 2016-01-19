-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Execute entries from the Great Language Shootout using R, quasiquotes and
-- compare the output.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Test.Scripts

import H.Prelude as H hiding (show)
import Language.R.QQ

import Control.Monad (forM)
import Control.Memory.Region
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import System.IO
import System.IO.Silently (capture_)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

inVoid :: R V s -> R V s
inVoid = id

main :: IO ()
main = do
    let qqs =
          $(do exps <- forM scripts $ \script -> do
                 TH.runIO (readFile script) >>= TH.quoteExp r
               return $ TH.ListE exps)
    H.withEmbeddedR H.defaultConfig $ defaultMain $
      testGroup "Quoted shootout programs" $
        zipWith cmp scripts qqs
  where
    cmp script qq = testCase script $ do
      x <- readProcess "R" ["--slave"] =<< readFile script
      y <- capture_ $ H.unsafeToIO $ inVoid qq
      x @=? y
