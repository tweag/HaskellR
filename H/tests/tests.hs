{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Contains golden tests. To reset the expected output with the actual output,
-- run with @--accept@ or remove the golden file that you want to reset.

module Main where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.R.Instance as R
import System.Environment (setEnv)
import System.IO
import System.Process
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Prelude -- silence AMP warning

-- | Pipe input to given command and return stdout, stderr.
cat :: FilePath -> [String] -> FilePath -> IO (Text, Text)
cat cmd args fp = do
    inh <- openFile fp ReadMode
    (_, Just outh, Just errh, _) <- createProcess $ (proc cmd args)
      { std_in = UseHandle inh
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
    (,) <$> Text.hGetContents outh <*> Text.hGetContents errh

invokeH :: FilePath -> IO (Text, Text)
invokeH = cat "H" []

gold :: TestName -> FilePath -> IO Text -> TestTree
gold name golden action =
    goldenTest
      name
      (Text.readFile golden)
      action
      (\(scrub -> expected) (scrub -> actual) ->
       if Text.words expected == Text.words actual
       then return Nothing
       else return $ Just $
              unlines ["Outputs don't match."
                      , "Expected:"
                      , Text.unpack expected
                      , ""
                      , "Actual:"
                      , Text.unpack actual
                      ])
      (Text.writeFile golden)
  where
    scrub = Text.replace "\r\n" "\n"

tests :: TestTree
tests = testGroup "Integration tests"
  [ gold
      "qq"
       "tests/qq.ghci.golden.out"
       (fst <$> invokeH "tests/qq.ghci")
  ]

main :: IO ()
main = do
    -- Normalize internalization settings.
    setEnv "LC_ALL" "C"
    _ <- R.initialize R.defaultConfig
    defaultMain tests
