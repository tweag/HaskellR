-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Tests. Run H on a number of R programs of increasing size and complexity,
-- comparing the output of H with the output of R.

module Main where

import qualified H

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden.Advanced
import Test.Tasty.Golden.Manage

import System.IO
import System.Process
import System.FilePath

import Control.Monad.Trans
import Control.Applicative ((<$>))
import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

invokeR :: FilePath -> ValueGetter r Text
invokeR fp = do
    inh <- liftIO $ openFile fp ReadMode
    (_, Just outh, _, _) <- liftIO $ createProcess $ (proc "R" ["--vanilla"])
      { std_out = CreatePipe
      , std_in = UseHandle inh
      }
    liftIO $ T.pack <$> hGetContents outh

invokeH :: FilePath -> ValueGetter r Text
invokeH fp =
    -- TODO once translated, Haskell source code should be executed somehow and
    -- output collected.
    liftIO $ H.translate <$> T.readFile fp

scriptCase :: TestName
           -> FilePath
           -> TestTree
scriptCase name scriptPath =
    goldenTest
      name
      (invokeR scriptPath)
      (invokeH scriptPath)
      (\outputR outputH ->
         if grepRValues (T.lines outputR) == T.lines outputH
         then return Nothing
         else return $ Just $ unlines ["Outputs don't match."
                                      , "R: "
                                      , T.unpack outputR
                                      , "H: "
                                      , T.unpack outputH
                                      ])
      (const $ return ())
  where
    -- | Filter printed values from R session.
    grepRValues :: [Text] -> [Text]
    grepRValues = filter (T.isPrefixOf "[1]")

integrationTests :: TestTree
integrationTests = testGroup "Integration tests"
  [ scriptCase "Trivial (empty) script" $
      "tests" </> "R" </> "empty.R"
  -- TODO: enable in relevant topic branches.
  -- , scriptCase "Simple arithmetic" $
  --     "tests" </> "R" </> "arith1.R"
  -- , scriptCase "Functions - factorial" $
  --     "tests" </> "R" </> "fact.R"
  -- , scriptCase "Functions - Fibonacci sequence" $
  --     "tests" </> "R" </> "fib.R"
  ]

tests :: TestTree
tests = testGroup "Tests" [integrationTests]

main :: IO ()
main = defaultMain tests
