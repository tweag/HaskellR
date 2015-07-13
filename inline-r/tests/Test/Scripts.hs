-- | List of shootout programs to test. In its own module due to TH stage
-- restriction.

module Test.Scripts where

import System.FilePath

scripts :: [FilePath]
scripts = map ("tests/shootout" </>)
    [ "binarytrees.R"
--  , "fannkuchredux.R" -- XXX takes long
    , "fasta.R"
    , "fastaredux.R"
--  , "knucleotide.R" -- XXX seems to require command line arguments
    , "mandelbrot-noout.R"
--  , "mandelbrot.R"  -- XXX produces some binary output which causes readProcess to fail
    , "nbody.R"
    , "pidigits.R"
--  , "regexdna.R" -- XXX seems to require command line arguments
--  , "reversecomplement.R" -- XXX seems to require command line arguments
    , "spectralnorm-math.R"
    , "spectralnorm.R"
    ]
