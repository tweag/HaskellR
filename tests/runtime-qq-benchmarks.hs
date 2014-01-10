-- Copyright: (C) 2013 Amgen, Inc.
--
-- This program executes the benchmark of the fib function using R and
-- the runtime qq.
--

import System.FilePath
import System.Process

-- | Runs a test using the R interpreter.
--
-- Returns the standard output.
runRTest :: FilePath -> IO String
runRTest fp = readFile fp >>= readProcess "R" ["--slave"]

-- | Runs a test using the runtime qq.
--
-- Returns the standard output.
runQQTest :: FilePath -> IO String
runQQTest fp = readFile fp >>= readProcess "sh" [ "tests" </> "ghciH.sh", "-v0", "-ghci-script", "H.ghci" ]

main :: IO ()
main = do
    (runQQTest $ "tests" </> "ghci" </> "qq-benchmarks.ghci") >>= putStrLn
    (runRTest $ "tests" </> "R" </> "fib-benchmark.R") >>= putStrLn
