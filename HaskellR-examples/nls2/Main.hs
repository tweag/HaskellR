-- | Dummy module intended to inform the user how this example should
-- be run.
module Main where

import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn $ concat [ "This example is intended to be executed by feeding"
                    , " the script into the H interpretter, e.g."
                    , " 'H -- -ghci-script nls2.hs'"
                    ]
  exitFailure
