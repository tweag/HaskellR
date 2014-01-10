-- Copyright: (C) 2013 Amgen, Inc.
--
-- This program executes shootout tests using R, runtime and compile-time qqs
-- and compares the outputs.
--

import Control.Exception

import System.Exit
import System.FilePath
import System.IO
import System.Process

-- | Runs a tests using the compile-time qq.
--
-- Returns the standard output.
runCompileQQTest :: FilePath -> IO String
runCompileQQTest fp = do
    let qqhs = "dist" </> "build" </> "cqq.hs"
    writeFile qqhs qqProgram
    callProcess "sh" ["tests" </> "ghcH.sh", "-threaded", qqhs ]
    readProcess ("dist" </> "build" </> "cqq") [] ""
  where
    qqProgram = unlines $
      [ "-- Copyright: (C) 2013 Amgen, Inc."
      , ""
      , "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE TemplateHaskell #-}"
      , "import H.Prelude"
      , "import Language.R.QQ"
      , "import Control.Monad ( void )"
      , ""
      , "import Language.Haskell.TH.Quote"
      , "import System.IO.Unsafe"
      , ""
      , ""
      , "main :: IO ()"
      , "main = runR defaultConfig $"
      , "    void $(quoteExp r $ unsafePerformIO $ readFile " ++ show fp ++ ")"
      ]

-- | Runs a test using the R interpreter.
--
-- Returns the standard output.
runRTest :: FilePath -> IO String
runRTest fp = readFile fp >>= readProcess "R" ["--slave"]

-- | Runs a test using the runtime qq.
--
-- Returns the standard output.
runQQTest :: FilePath -> IO String
runQQTest fp = readProcess "sh" [ "tests" </> "ghciH.sh", "-v0", "-ghci-script", "H.ghci" ] qqScript
  where
    qqScript = unlines $
      [ "-- Copyright: (C) 2013 Amgen, Inc."
      , ""
      , ":set -XTemplateHaskell"
      , "import Control.Monad ( void )"
      , ""
      , "import Language.Haskell.TH.Quote"
      , "import System.IO.Unsafe"
      , ""
      , ""
      , "void $(quoteExp H.r $ unsafePerformIO $ readFile " ++ show fp ++ ")"
      ]

compareOutputs :: FilePath -> IO Bool
compareOutputs fp = do
    putStr $ "testing " ++ fp ++ ": "
    hFlush stdout
    oR  <- runRTest fp
    oQQ <- runQQTest fp
    oCQQ <- catch (runCompileQQTest fp) (\e -> const (return "") (e :: SomeException))
    if oR == oQQ
    then if oR == oCQQ
      then putStrLn "OK" >> return True
      else do
        putStrLn $ unlines $
          [ "FAIL"
          , ""
          , "R:"
          , oR
          , ""
          , "compile-time QQ:"
          , oCQQ
          ]
        return False
    else do
      putStrLn $ unlines $
        [ "FAIL"
        , ""
        , "R:"
        , oR
        , ""
        , "QQ:"
        , oQQ
        ]
      return False


main :: IO ()
main = do
    res <- mapM compareOutputs $ map (("tests" </> "shootout") </>) rPrograms
    let passed = length $ filter id res
    if passed == length rPrograms
    then putStrLn $ "All " ++ show (length rPrograms) ++ " tests passed!"
    else do
      putStrLn $ show passed ++ " of " ++ show (length rPrograms)
                 ++ " tests passed."
      exitFailure
  where
    rPrograms =
        [ "binarytrees.R"
--        , "fannkuchredux.R" -- XXX takes long
        , "fasta.R"
        , "fastaredux.R"
--        , "knucleotide.R" -- XXX seems to require command line arguments
        , "mandelbrot-noout.R"
--        , "mandelbrot.R"  -- XXX produces some binary output which causes readProcess to fail
        , "nbody.R"
        , "pidigits.R"
--        , "regexdna.R" -- XXX seems to require command line arguments
--        , "reversecomplement.R" -- XXX seems to require command line arguments
        , "spectralnorm-math.R"
        , "spectralnorm.R"
        ]
