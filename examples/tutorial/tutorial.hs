{-# LANGUAGE QuasiQuotes, ViewPatterns, ScopedTypeVariables, GADTs, DataKinds #-}

-- |
-- Copyright (C) 2023 by Dominick Samperi
-- License: BSD-3-Clause

module Main where
  
import qualified Language.R as R
import Language.R (R)
import Language.R.QQ

import qualified H.Prelude as H
import qualified H.Prelude.Interactive as I

import Math.List.FFT
import Math.List.Wavelet

import Control.Monad(when)
import Data.Complex
import Data.Int (Int32) -- R uses 32-bit ints, not 64-bit
import System.IO

import qualified Data.Vector.SEXP as V
import qualified Data.Vector.SEXP.Mutable as M
import Control.Memory.Region -- for V

import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Binary.Get
import Data.Binary.IEEE754
import Control.Monad

-- Convenience types for working with proxy vectors
type RProxyVectorDouble = V.Vector 'R.Real Double
type RProxyVectorInt    = V.Vector 'R.Int  Int32

-- Cast Double list to Complex Double list
toDoubleComplex :: [Double] -> [Complex Double]
toDoubleComplex = map (:+ 0)

-- Interface to R's fft()
r_fft :: [Complex Double] -> R s [Complex Double]
r_fft vec = do
  R.dynSEXP <$> [r| fft(vec_hs) |]
  
-- Interface to R's 2-dimensional fft()
r_fft2d :: [Complex Double] -> Int -> Int -> R s [Complex Double]
r_fft2d mvec nrows ncols = do
  let nrows32 = fromIntegral nrows ::Int32
      ncols32 = fromIntegral ncols ::Int32
  R.dynSEXP <$> [r| fft(matrix(mvec_hs, nrows32_hs, ncols32_hs)) |]
  
-- Database of examples: index, description, and function.
-- Functions are defined after main below, followed by 
-- implementations for utilities like the cummulative 
-- and the complex complex gamma function.  
db = [ (1,("Introduction to tutorial",intro))
     , (2,("Fetch R's home directory",getrhome))
     , (3,("Work with data frames and global env", dataframe))
     , (4,("Simple R plot",simpleplot))
     , (5,("Splicing into and extracting from quasiquotes",splicing))
     , (6,("Lift a scalar function to operate on vectors",lifting))
     , (7,("Haskell proxy vector interface to R",proxyvector))
     , (8,("Use R's 1d and 2d fft",rfft))
     , (9,("3d plot of complex gamma function", gamma3d))
     , (10,("Compute shifted Gaussian FFT",gaussian))
     , (11,("Read data written by R's saveRDS",readrds))
     , (12,("Read data written by R's writeBin",readbin))
     , (13,("Use R's optimizer",optimx))
     , (14,("Compute Black-Scholes implied volatility",bsImpliedVol))
     , (15,("Compute analytic signal and Hilbert transform",analyticSine))
     , (16,("Compute classic D4 wavelet", waveletD4))
       
     , (17,("Use Rcpp to compile and use C++ code on the fly",rcpp))
     , (18,("FM radio signal spectral analysis and demodulation",fmiq))
     ]

-- Presents list of examples for the user to select from.
-- Terminates on an empty line.
exampleProcessor :: IO ()
exampleProcessor = do
  let num = map fst db  
      desc = map (fst . snd) db
      select = zip num desc
  putStrLn ""
  mapM_ print select    
  putStr "\nTut> "
  hFlush stdout
  line <- getLine
  when(line /= "") $ do
    let result = lookup (read line::Int) db
    case result of
      Just (a,b) -> b
      Nothing -> putStrLn "Didn't get that, try again..."
    exampleProcessor
  
-- Start embedded R instance and example processor.
main :: IO ()
main = H.withEmbeddedR H.defaultConfig $ do
  exampleProcessor

-- The examples...  
    
intro :: IO ()    
intro = do
  putStrLn $ unlines
    ["\nHakellR provides a Haskell interface to R by permitting developers"
    , "to embed R code in Haskell using special constructions called"
    , "quasiquotes, for example, the quasiquote [r| sqrt(1:4) |] embeds"
    , "a call to R's sqrt() function, and [r| sqrt(vec_hs) |] embeds"
    , "a call that passes a Haskell variable named vec to the"
    , "R call. The quasiquote [r| func_hs(2) |] passes a Haskell"
    , "function named func to be used in an R expression."
    , ""  
    , "HaskellR expands quasiquotes (part of template Haskell) at"    
    , "compile time, inserting Haskell code that makes calls to R"  
    , "as needed. In this way the programmer can speak with R in its"
    , "own language, without the need for an intermediate DSL."  
    , ""  
    , "At runtime each quasiquote evaluates to an R SEXP, inside"  
    , "a container called the R monad. The contents can be printed"
    , "just as R would using something like:"
    , ""
    , "> [r| sqrt(2) |] >>= I.printR"
    , ""
    ]
  [r| sqrt(2) |] >>= I.printR
  putStrLn $ unlines
    [
      "\nThis extracts the SEXP and applies R's print function. A"  
    , "convenience function I.p can be used to do the same thing using:"  
    , ""
    , ">   I.p [r| sqrt(2) |]"  
    , ""
    , "The first variant (using >>=) is equivalent to the following, which"
    , "is just 'syntactic sugar,' but might be more meaningful"
    , "for imperative programmers:"
    , ""
    ,  "> do"  
    ,  ">    x <- [r| sqrt(2) |]"
    ,  ">    I.printR x"
    , ""    
    , "In some of the examples below there is no need to print the"  
    , "result of the quasiquote, as we are only interested in a"
    , "side-effect, like the creation of a plot. In this caase the"  
    , "quasiquote can be written on a line by itself, but this cannot"
    , "be the last statement of a block, so we append a return statement."  
    , "Note that these return statements are not transfer of control"
    , "constructions as is the case in non-functional languages."
    , ""  
    , "For best results work with the tutorial interface at a"
    , "workstation terminal along with the source code. Note that 'r'" 
    , "should not be used as the name of a local variable, as" 
    , "this will conflict with quasiquotes. Also, the 'r' in a" 
    , "quasiquote should be immediately followed by '|' (no space)" 
    , "to prevent a syntax error."
    ]  
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()  
    
getrhome :: IO ()
getrhome = do
  putStrLn $ unlines
    [ "\nWe can install R packages, load libraries, and run"
    , "arbitrary R code in a quasiquote. Let's find R's home" 
    , "directory..."
    , "\n> I.p [r| R.home() |]\n"
    ]
  I.p [r| R.home() |]  

dataframe :: IO ()
dataframe = do
  putStrLn $ unlines     
    [ "\nHere we create and display a data frame in R, and"
    , "place the name in R's global environment using '<<-',"
    , "so it can be referenced in other quasioquotes."
    , "Then we inspect the 'x' column from another quasiquote."
    , ""
    , "> I.p [r| mydf <<- data.frame(x=1:5,y=6:10) |]"
    , "> I.p [r| mydf$v |] -- use data frame in global env."
    ]
  I.p [r| mydf <<- data.frame(x=1:5,y=6:10) |]
  I.p [r| mydf$x |]
  putStrLn $ unlines
    [ "\nAlternatively, we could capture the 'x' column in a Haskell"
    , "list for further processing (see below on how to do this)."
    ]

simpleplot :: IO ()
simpleplot = do
  putStrLn $ unlines
    [ "\nThe following example uses R to generate a simple plot\n"
    , "[r| plot(0:200/20, sin(0:200/20), type='l') |]"
    , "return ()\n"
    , "Notice that we do not use printR here because we are only"
    , "interested in the side-effect (the plot), but the block must"
    , "be terminated with return ()\n"
    ]
  [r| plot(0:200/20, sin(0:200/20), type='l',main='Simple Plot') |]
  return ()

proxyvector :: IO ()
proxyvector = do
  putStrLn $ unlines
    [ "\nIt is possible to create proxy interfaces to R vectors that"
    , "can be manipulated using the Haskell vector API. These vectors can"
    , "be passed to an R quasiquote using the \"_hs\" suffix. Example...\n"
    , "let vd = V.fromList [1..5] :: RProxyVectorDouble"
    , "    vi = V.fromList [1..5] :: RProxyVectorInt"
    , "I.p [r| vd_hs + vi_hs |]\n"
    ]
  let vd = V.fromList [1..5] :: RProxyVectorDouble
      vi = V.fromList [1..5] :: RProxyVectorInt
  I.p [r| vd_hs + vi_hs |]
  putStrLn $ unlines
    [ "\nThe result of an R quasiquote can be captured in a Haskell"
    , "list (Double or Int32). This list could be transformed on the"
    , "Haskell side, and the result could be passed to subsequent"
    , "R quasiquotes. Example..." 
    , "x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| sqrt(1:5) |]"
    , "y::[Int32] <- R.runRegion $ R.dynSEXP <$> [r| as.integer(1:5) |]"
    , "print x"
    , "print y\n"
    ]
  x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| sqrt(1:5) |]
  y::[Int32] <- R.runRegion $ R.dynSEXP <$> [r| as.integer(1:5) |]
  print x
  print y             
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()
  putStrLn $ unlines
    [ "\nA mutable copy of an immutable vector (like vd) can be"
    , "created using the V.thaw function. It can then be modfied using"
    , "M.read and M.write. But before the result can be passed to subsequent"
    , "quasiquotes it needs to be frozen into an immutable vector. Example..."
    ,  "mvd <- V.thaw vd"
    ,  "M.write mvd 0 3.14 -- modify first element"
    ,  "vd2 <- V.freeze mvd -- freeze immutable copy that can be passed to R"
    ,  "I.p [r| vd2_hs + vi_hs |]"
    ]
  mvd <- V.thaw vd
  M.write mvd 0 3.14 -- modify first element
  vd2 <- V.freeze mvd -- freeze immutable copy that can be passed to R
  I.p [r| vd2_hs + vi_hs |]
  putStrLn $ unlines
    [ "\nMutable vector proxies can also be created as follows, but"
    , "the API is more complicated in this case. The vector must be"
    , "'released' before it can be passed to a quasiquote, and changes"
    , "made to the vector in the quasiqoute do not change the mutable"
    , "vector on the Haskell side (unlike the default behavior of Rcpp).\n"
    , "R.runRegion $ do"
    , "  mx3 <- M.new 3 :: R s (M.MVector s 'R.Real Double)"
    , "  M.unsafeWrite mx3 0 1"
    , "  M.unsafeWrite mx3 1 2"
    , "  M.unsafeWrite mx3 2 3"
    , "  let mx3release = M.release mx3 :: M.MVector V 'R.Real Double"
    , "  I.p [r| mx3release_hs |]"
    ]
  R.runRegion $ do
    mx3 <- M.new 3 :: R s (M.MVector s 'R.Real Double)
    M.unsafeWrite mx3 0 1
    M.unsafeWrite mx3 1 2
    M.unsafeWrite mx3 2 3
    let mx3release = M.release mx3 :: M.MVector V 'R.Real Double
    I.p [r| mx3release_hs |]
  
splicing :: IO ()
splicing = do  
  putStrLn $ unlines
    [ "\nThe use of the '_hs' suffix to splice haskell variables into R code"
    , "is similar in function to the Rcpp::wrap() function, in that it maps"
    , "from Haskell to R. Only a few Haskell types can be spliced in this way,"
    , "but support for new types can be added using the Literal typeclass."
    , "(This typeclass has conversion functions mkSEXP and fromSEXP, similar to"
    , "Rcpp::wrap and Rcpp::as<T>, respectively.)"
    , ""      
    , "Fecthing Haskell values out of a quasiquote is"  
    , "similar in function to Rcpp::as<T>, as this maps a SEXP to a"
    , "Haskell value."
    , ""
    , "Here is sample code that splices in a Double vector and a function:"
    , "Note the use of return along with the special wrapped type to define a"
    , "Haskell function that can be passed to R."
    , ""  
    , "> let hvec = [1,2,3] :: [Double]"
    , ">     hfunc x = return (sqrt x) :: R s Double"
    , "> I.printR =<< [r| c(sum(hvec_hs),hfunc_hs(2.0)) |] -- two splices into R"
    ]
  let hvec = [1,2,3] :: [Double]
      hfunc x = return (sqrt x) :: R s Double
  I.p [r| c(sum(hvec_hs),hfunc_hs(2.0)) |] -- two splices into R
  putStrLn $ unlines
    [ "\nHere is sample code that extracts a Haskell list from a quasiquote:"
      , "\n> x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| sqrt(1:4) |]"
      , "> print x -- Haskell's print function"
    ]
  x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| sqrt(1:4) |] -- R vector -> Haskell list
  print x -- Haskell's print function               
  putStrLn $ unlines  
    [ "\nHere is another example where automatic type conversion and"
    , "managing invalid data is illustrated"
    , "\n> x1::[Double] <- R.runRegion $ R.dynSEXP <$> [r| list(1,2,'c',4) |]\n"
    ]  
  x1::[Double] <- R.runRegion $ R.dynSEXP <$> [r| list(1,2,'c',4) |]
  print x1
  putStrLn $ unlines
    [ "\nThe function of R.dynSEXP is to cast the SEXP into a suitable"
    , "type (inferred from context), and the function of R.runRegion"
    , "is to force full evaluation (Haskell is a lazy language)."
    , "The <$> operator permits R.dynSEXP to work on the contents of"
    , "the container that the quasiquote evaluates to."
    ]
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()
  
lifting :: IO ()
lifting = do
  putStrLn $ unlines
    [ "\nAfter defining a Haskell function like f below to be used in"
    , "a quasiquote, how do we define a similar function that works on"
    , "a vector of Double's instead of a single Double? This is done"
    , "by 'lifting' the function so it can be applied to each element of"  
    , "a list in turn, and the final result is wrapped in an R monad"
    , "container. (This defines the operation of a functor.) Here"
    , "is an example:\n"
    , ""
    , "> let v = [1,2,3,4]::[Double]"
    ,  ">     f x = return(sqrt x) :: R s Double"
    ,  ">     fLift = Prelude.mapM f :: [Double] -> R s [Double]"
    ,  "> I.p [r| fLift_hs(v_hs) |]"
    ,  "> I.p [r| fLift_hs(c(2,4,6,8,10)) |]\n"
    ]
  let v = [1,2,3,4]::[Double]
      f x = return(sqrt x) :: R s Double
      fLift = Prelude.mapM f :: [Double] -> R s [Double]
  I.p [r| fLift_hs(v_hs) |]
  I.p [r| fLift_hs(c(2,4,6,8,10)) |]
  return ()

printMatrix :: [Complex Double] -> Int -> Int -> IO ()
printMatrix mvec nrows ncols = mapM_ printRow [0..(nrows-1)]
   where
     get i j = mvec!!(j*nrows+i)
     
     printRow :: Int -> IO ()
     printRow i = do
       mapM_ (printCell i) [0..(ncols-1)]
       putStrLn "\n"
       
     printCell :: Int -> Int -> IO ()
     printCell i j = putStr (show (get i j) ++ " ")

rfft :: IO ()
rfft = do
  putStrLn $ unlines
    [ "\nR built-in functions can be configured for use in Haskell. For"
    , "this purpose, use R.dynSEXP to define a callable interface"
    , "as we do for R's fft above (see definition of r_fft and r_fft2d)."
    , "Such functions are used from Haskel as follows."
    , "\n> x <- R.runRegion $ r_fft [1,2,3,4]\n"
    , "> print x -- R's print function"  
    ]
  x <- R.runRegion $ r_fft [1,2,3,4]
  print x -- Here x is a Haskell object, and print is std print in Haskell.
  putStrLn $ unlines
    [ "\nFor the 2D case, we start by creating 1D vector and 2D subscript operator `mind`..."
    , "> let nrows=3"
    , ">     ncols=4"
    , ">     -- A vector of complex numbers to be used like a m atrix in R."
    , ">     mvec = [z | i <- [0..(nrows*ncols-1)], let z = fromIntegral i + (1:+0)]"
    , ">     -- mind for 2d matrix indexing"
    , ">     mind x (i,j) = x!!(i + j*nrows) :: (Complex Double)\n"
    ,  "Next we call R's fft with vector and dimensions..."
    , "> mvecfft <- R.runRegion $ r_fft2d mvec nrows ncols"
    , "> print mvexfft"
    ]  
  let nrows=3
      ncols=4
      -- A vector of complex numbers to be used like a matrix in R
      mvec = [z | i <- [0..(nrows*ncols-1)], let z = fromIntegral i + (0:+0)]
      -- mind for 2d matrix indexing
      mind x (i,j) = x!!(i + j*nrows) :: (Complex Double)
  mvecfft <- R.runRegion $ r_fft2d mvec nrows ncols
  print mvecfft
  putStrLn $ unlines
    [ "\nUse the matrix indexing operator..."
    , "> putStrLn $ \"fft(0,0) = \" ++ show (mvecfft `mind` (0,0))"
    , "> putStrLn $ \"fft(0,3) = \" ++ show (mvecfft `mind` (0,3))"
    , "> putStrLn $ \"fft(2,0) = \" ++ show (mvecfft `mind` (2,0))"
    ]
  putStrLn $ "fft(0,0) = " ++ show (mvecfft `mind` (0,0))
  putStrLn $ "fft(0,3) = " ++ show (mvecfft `mind` (0,3))
  putStrLn $ "fft(2,0) = " ++ show (mvecfft `mind` (2,0))
  putStrLn $ unlines
    [ "\nPrint the entire matrix returned by r_fft2d and compare with direct call to R..."
    , "> printMatrix mvecfft nrows ncols"
    ]
  printMatrix mvecfft nrows ncols -- should match R's output
  
  -- Cannot pass Haskell's 64bit Int's to R.
  let nrows32 = fromIntegral nrows :: Int32
      ncols32 = fromIntegral ncols :: Int32
  I.p [r| fft(matrix(0:((nrows32_hs*ncols32_hs)-1),nrows32_hs,ncols32_hs)) |]
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()  

optimx :: IO ()
optimx = do
  putStrLn $ unlines
    [ "\nDefine the function f(x,y) = (x-1)^2 + (y-2)^2 and its" 
    , "gradient as Haskell functions, to be passed to R's optimx"
    , "to find min f(x,y). Exact solution is obviously (1,2)."
    , "This function sometimes triggers harmless warnings about min(x)."
    , "Here are the results from the optimizer. The BFGS answer is"
    , "(p1,p2) = (1.0,2.0) as expected."
    ]
  let fn x y = return ((x - 1.0)^2 + (y - 2.0)^2) :: R s Double
      gr x y = return ([2*(x - 1.0), 2*(y - 2.0)]) :: R s [Double]
      startXY = [0.0, 0.0] :: [Double] -- initial guess
  -- TODO: For simplicity we are simply letting R print the results
  -- returned by optimx(). It would be more useful to return a    
  -- Haskell vector/list so we can do further processing.
  I.p [r| library(optimx)
          fn <- function(v) fn_hs(v[1], v[2])
          gr <- function(v) gr_hs(v[1], v[2])      
          optimx(startXY_hs, fn, gr, control=list(reltol=1e-12)) |]
  putStrLn $ unlines
    [ "\nWe could extract selected results into a"
    , "Haskell vector/list with status information, to be used for"
    , "further processing."
    ]    
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()

data OptionType = Call | Put deriving(Show,Enum)

bsImpliedVol :: IO ()
bsImpliedVol = do
  putStrLn $ unlines
    [ "\nCompute Black-Scholes implied volatility given price."
    , "The commulative normal distribution cumNorm(x) is defined below"
    , "to avoid package dependencies. The root is $root = 0.160595,"
    , "so the implied volatility is 16.06%."
    ]
  let optType = Call
      optPrice = 2.0 -- option price is $2
      spot = 100.0 -- current (spot) price ($100)
      strike = 100.0  -- strike price
      irate = 0.05 -- interest rate (don't use 'r' here!, will conflict with 'r' in quasiquote)
      drate = 0.01 -- dividend rate
      tm = 30.0/365.25  -- time to maturity in years (30 days)
      rateDisc = exp(-irate*tm) -- interest rate discount
      divdDisc = exp(-drate*tm) -- dividend discount
      forward = spot*divdDisc/rateDisc -- forward price
      rootT = sqrt tm
      price sigma = case optType of
        Call -> rateDisc * (forward*cumNorm d1 - strike*cumNorm d2)
        Put  -> rateDisc * (strike*cumNorm (-d2) - forward*cumNorm (-d1))
        where
          d1 = log(forward/strike)/sigma/rootT + 0.5 * sigma*rootT
          d2 = d1 - sigma*rootT
      priceDiff sigma = return (price sigma - optPrice) :: R s Double
      searchInterval = [0.01 ,1.0] :: [Double] -- interval to search
  -- TODO: For simplicity we are simply letting R print the results
  -- returned by uniroot(). It would be more useful to return a    
  -- Haskell vector/list that can be used for further processing.
  I.p [r| uniroot(priceDiff_hs, searchInterval_hs) |]
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()

readrds :: IO ()
readrds = do
  putStrLn $ unlines
    [ "\nRead data written by R's saveRDS (R Data Serialization format)."
    , "R code to save data...\n"
    , "[r| z = as.complex(1:5) + 0.5i"
    , "    x = as.double(1:5)"
    , "    saveRDS(z, \"data/complex.rds\")"
    , "    saveRDS(x, \"data/double.rds\") |]"
    ]
  [r| z = as.complex(1:5) + 0.5i
      x = as.double(1:5)    
      saveRDS(z, "data/complex.rds")
      saveRDS(x, "data/double.rds") |]
  putStrLn $ unlines
    [ "\nRead data into Haskell lists and print...\n"
    , "x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| readRDS(\"data/double.rds\") |]"
    , "z::[Complex Double] <- R.runRegion $ R.dynSEXP <$> [r| readRDS(\"data/complex.rds\") |]"
    , "putStrLn $ \"x = \" ++ show x"
    , "putStrLn $ \"z = \" ++ show z\n"
    ]
  x::[Double] <- R.runRegion $ R.dynSEXP <$> [r| readRDS("data/double.rds") |]
  z::[Complex Double] <- R.runRegion $ R.dynSEXP <$> [r| readRDS("data/complex.rds") |]
  putStrLn $ "x = " ++ show x
  putStrLn $ "z = " ++ show z
    
readbin :: IO ()
readbin = do
  putStrLn $ unlines
    [ "\nWrite data using R's writeBin (IEEE754),"
    , "then read it as a Haskell list. R code...\n"
    , "[r| z = as.complex(1:5) + 0.5i"
    , "    x = as.double(1:5)"
    , "    writeBin(z, \"data/complex.bin\")"
    , "    writeBin(x, \"data/double.bin\") |]"
    ]
  [r| z = as.complex(1:5) + 0.5i
      x = as.double(1:5)    
      writeBin(z, "data/complex.bin")
      writeBin(x, "data/double.bin") |]
  putStrLn $ unlines
    [ "\nHaskell code...\n"
    , "doubles <- readDoubles \"data/double.bin\""    
    , "complex <- readComplexDoubles \"data/complex.bin\""
    , "putStrLn $ \"x = \" ++ show doubles"
    , "putStrLn $ \"z = \" ++ show complex"
    ]
  doubles <- readDoubles "data/double.bin"
  putStrLn $ "x = " ++ show doubles
  complex <- readComplexDoubles "data/complex.bin"
  putStrLn $ "z = " ++ show complex
  
-- Rcpp must be added to the list of R packages required in
-- shell.nix
rcpp :: IO ()
rcpp = do
  putStrLn $ unlines       
    [ "\nUse Rcpp to compile and run C++ code on the fly."
    , "This is done inside the following quasiquote. It also" 
    , "includes R code to test the compiled function."
    , "Note that vector arguments are updated in place by"
    , "default (use clone to prevent mods to the input vectors).\n"
    , "I.p [r| library(Rcpp)"
    , "Rcpp::sourceCpp(code='"
    , "  #include <Rcpp.h>"
    , "  // [[Rcpp::export()]]"
    , "  SEXP cpptest(Rcpp::NumericVector v, Rcpp::NumericVector w) {"
    , "    Rcpp::NumericVector wcopy = Rcpp::clone(w);"
    , "    for(int i = 0; i < v.size(); ++i) {"
    , "      v(i) = 100.0 + i; // update in place by default"
    , "      wcopy(i) = 200.0 + i;"
    , "    }"
    , "  return wcopy;"
    , "  }'"
    , ")"
    , "## R code that uses C++ function defined above"
    , "v <- as.double(1:5)"
    , "w <- as.double(1:5)"
    , "list(returnvalue=cpptest(v,w),"
    , "v=v, w=w) |]"
    ]
  I.p [r| library(Rcpp)
  Rcpp::sourceCpp(code='
    #include <Rcpp.h>
    // [[Rcpp::export()]]
    SEXP cpptest(Rcpp::NumericVector v, Rcpp::NumericVector w) {
      Rcpp::NumericVector wcopy = Rcpp::clone(w);
      for(int i = 0; i < v.size(); ++i) {
        v(i) = 100.0 + i; // update in place by default
        wcopy(i) = 200.0 + i;
      }  
    return wcopy;
    }'
  )
  ## R code that uses C++ function defined above
  v <- as.double(1:5)
  w <- as.double(1:5)
  list(returnvalue=cpptest(v,w),
  v=v, w=w) |]
  putStrLn "Press Enter to continue"
  _ <- getLine
  return ()

fmiq :: IO ()
fmiq = do
  putStrLn $ unlines
    [ "\nRead FM radio IQ data downloaded from pySDR online book website"
    , "and perform a spectral analysis of the corresponding demodulated"
    , "signal. The corresponding audio clip (4 seconds duration) is saved to"
    , "tutorial.wav. Uses R's spectrum() function to estimate the spectral"
    , "density. For more info on IQ see the Hackage"
    , "documentation for the Math.List.FFT package (analytic function)."
    , ""
    , "Spectral power is concentrated at 0Hz (Mono), 38k (Stereo),"
    , "19k (Pilot tone), 57k (RDS), and 67k (SCA). The second SCA band (92k)"
    , "is not evident in this signal. The spike at 76k is the fourth"
    , "harmonic of the pilot tone. Most of this would be visible"
    , "in a spectral analysis of the raw signal."
    ]
  [r| library(signal) ## for unwrap() and decimate()
      library(tuneR)  ## to save .wav file
      
      ## Read FM radio IQ data from pySDR online book.
      ## The station frequency is not really needed because it has                         
      ## already been down-converted to 0 Hz.
      freq <- 99.5*1e6 ## 99.5 MHz down-converted to 0 Hz.
      fs <- 250000 ## sample rate 250k
      samples <- 1000000 ## 1M IQ samples

      ## download.file() does not work in a quasiquote (SSL connect error).
      ## Use a browser to download.
      ##url <- "https://github.com/777arc/498x/blob/master/fm_rds_250k_1Msamples.iq?raw=true"
      ##download.file(url,"examples/tutorial/fm2_rds_250k_1Msamples.iq")
           
      ## Read 2M doubles (1M complex numbers) saved using Python.
      fmiqraw <- readBin("examples/tutorial/fm_rds_250k_1Msamples.iq",
                         "numeric", size=4, n=2000000, endian = "little")
      realPart <- fmiqraw[seq(1,length(fmiqraw),2)]           
      imagPart <- fmiqraw[seq(2,length(fmiqraw),2)]
      fmiq <- complex(real=realPart,imaginary=imagPart)

      ## Simple demodulation (no deemphasis or stereo decoding)
      ## This approximates the derivative of the phase (FM).
      arg <- Arg(fmiq)
      un <- unwrap(arg)
      demod <- diff(un)

      ## Spectral analysis of demodulated signal
      ser <- ts(abs(demod), fs)
      sp <- spectrum(ser, kernel=kernel("daniell", 28), plot=FALSE)
      options(scipen=5) ## No scientific notation on axes
      plot(sp$freq*fs, sp$spec, type='l', log='y',
           xlab='Frequency', ylab='Power Spectral Density',
           main='Demodulated FM Radio Signal')
      color <- 'blue'
      voffset <- 100.0
      abline(v=0,lty=2,col=color); text(0, voffset, pos=4, 'L+R')
      abline(v=19000,lty=2,col=color); text(19000, voffset, pos=3, 'Pilot')
      abline(v=38000,lty=2,col=color); text(38000, voffset, pos=3, 'L-R')
      abline(v=57000,lty=2,col=color); text(57000, voffset, pos=3, 'RDS')
      abline(v=67000,lty=2,col=color); text(67000, voffset, pos=3, 'SCA')
      abline(v=92000,lty=2,col=color); text(92000, voffset, pos=3, 'SCA')

      ## Decimate since we don't care about anything above audio frequencies,
      ## and we don't want to create a huge .wav file. Note that Ubuntu
      ## Desktop may have a problem playing wav files (use play cmd or browser).
      decimateFactor=20 ## about 100/20 or 5 kHz is sufficient for mono audio.
      demod <- decimate(demod, decimateFactor, ftype='fir')
      fs <- fs/decimateFactor

      ## Normalize the result and save to audio file.
      maxdemod <- max(demod)
      mindemod <- min(demod)
      demod <- demod/max(maxdemod,-mindemod) ## normalize
      w <- Wave(demod, samp.rate=fs, bit=32, pcm=FALSE)
      writeWave(w,'data/tutorial.wav') |]
  return ()
  
gaussian :: IO ()
gaussian = do
  putStrLn $ unlines
    [ "\nUse fftshift to shift the fft of a Gaussian function so that"
    , "the zero frequency point is centered, and we get the well-known"
    , "result that the Fourier transform of a Gaussian is another Gaussian.\n"
    , "For more information about the mathematics involved see the the Hackage"
    , "documentation for Math.List.FFT."
    ]
  let n = 1024 -- number of sample points
      dt = 10.24/fromIntegral n -- time increment
      df = 1.0/dt/fromIntegral n -- frequency increment
      t = take n $ iterate (+ dt) 0 -- time grid
      f = take n $ iterate (+ df) 0 -- freq grid
      fs = 1.0/dt -- sampling rate
      signal t = exp(-64.0*t^2) -- Gaussian function
      gauss = map ((:+ 0) . signal) t -- sample and complexify
      ft = fft gauss -- standard unshifted fft
      mags = map magnitude ft -- modulus vector
      ftshift = fftshift ft  -- shifted (rotated) transform
      magshift = map magnitude ftshift -- modulus vector
      fshift = take n $ iterate (+ df) (-fs/2) -- shifted frequencies
  [r| save = par(mfrow=c(2,1))
         plot(f_hs, mags_hs, type='l',main='Uncentered FFT of Gaussian')
         plot(fshift_hs, magshift_hs,type='l',main='Centered FFT of Gaussian')
         par(save) |]       
  return ()
  
  
analyticSine :: IO ()  
analyticSine = do
  putStrLn $ unlines
    [ "\nCompute and display the analytc signal corresponding to a"
    , "sine wave. The real part is the original signal, and the"
    , "imaginary part is the Hilbert transform. Real and imaginary"
    , "parts are orthoginal to each other.\n"
    , "For more information on the mathematics see the Hackage"
    , "documentation for the package Math.List.FFT."
    ]
  let n = 1024 -- number of sample points
      dt = 2*pi/fromIntegral (n-1) -- time increment
      t = take n $ iterate (+dt) 0 -- time grid
      sig = [z | k <- [0..(n-1)], let z = sin (t!!k)] -- sine wave
      z = analytic sig -- analytic signal
      zr = map realPart z
      zi = map imagPart z
      innerProduct = sum $ zipWith (*) zr zi -- should be zero
  [r| plot(t_hs, zr_hs, xlab='t',ylab='signal',type='l',col='blue',
           main="Orig. signal blue, Hilbert transform red")
           lines(t_hs, zi_hs,type='l',col='red') 
      dev.flush() |]
  return ()
    
waveletD4 :: IO ()
waveletD4 = do
  putStrLn $ unlines
    [ "\nCompute and display the classic 4-coefficient mother wavelet"
    , "of Daubechies 1988.\n"
    , "For more information about the mathematics see the Hackage documentation"
    , "for Math.List.Wavelet."
    ]
  let sig = deltaFunc 5 1024 -- unit mass at position 5
      wavelet = iwt1d sig 2 0 10 -- inverse wavelet transform (N=2,1024=2^10)
  [r| plot(wavelet_hs,type='l',main="Daubechies wavelet (N=2)") |]
  return ()
  
-- Complex gamma function.
gamma3d :: IO ()
gamma3d = do
  putStrLn $ unlines
    [ "\nThis example illustrates how to pass a matrix of data from Haskell"
    , "to R. This is done by writing the 2d matrix into a 1d vector, one"
    , "column at a time, as this is the way R lays out 2d matrices in memory."
    , "Then R's matrix() function can be used to turn the 1d vector into a 2d"  
    , "matrix, suitable for use in the 3d perspective plot function persp().\n"
    , "In this example, the x and y coordinates are the real and imaginary"
    , "parts of complex numbers that are passed to the complex gamma function"
    , "(generalized factorial function), and the 2d function to"
    , "be plotted is the modulus of gamma(z). The complex gamma function"
    , "is implemented below.\n"
    ]  
  let nreal = 50
      nimag = 100
      realRange = 8.0
      imagRange = 4.0
      dreal = realRange/fromIntegral (nreal-1) :: Double
      dimag = imagRange/fromIntegral (nimag-1) :: Double
      rvec = take nreal $ iterate (+ dreal) (-realRange/2.0)
      ivec = take nimag $ iterate (+ dimag) (-imagRange/2.0)
      magvec = [z | j <- [0..(nimag-1)], i <- [0..(nreal-1)], 
                let z = magnitude $ gamma $ rvec!!i :+ ivec!!j]
      -- R stores 2d matrices by columns, so in m(i,j),  to
      -- run through the rows of each column, we need a nested
      -- loop where j is defined in the outer loop, and i is set
      -- in the inner loop. This corresponds to the order of the
      -- i and j assignments in the list comprehension.
  [r| magmatrix <- matrix(magvec_hs, 
                          nrow=length(rvec_hs), 
                          ncol=length(ivec_hs))
      persp(rvec_hs, ivec_hs, magmatrix,ticktype='detailed',theta=-40,
      main="Modulus of Complex Gamma",col='cyan',
      xlab='real',ylab='imag',zlab="|Gamma(z)|") 
      dev.flush() |]
  return ()
  
-- The commulative normal function and the complex gamma function...
  
-- |Cummulative normal distribution
-- Avoid package dependences by computing the cummulative normal
-- using Abramowitz and Stegun formula 7.1.26 (thanks to John D. Cook)        
-- and the identity cumNorm(x) = (1 + erf(x/sqrt 2))/2.        
cumNorm :: Double -> Double
cumNorm x = (2.0 - t*(a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))*exp(-x^2/2.0))/2.0
    where
        a1 =  0.254829592
        a2 = -0.284496736
        a3 =  1.421413741
        a4 = -1.453152027
        a5 =  1.061405429
        p  =  0.3275911
        t = 1.0/(1 + p*x/sqrt 2.0)

-- |Complex gamma function using Lanczos approximation (see Wikipedia).
gamma :: Complex Double -> Complex Double        
gamma z = if realPart z < 0.5 
          then
            pi/(sin(pi*z)*gamma(1.0-z))
          else
            val
  where g = 7 -- length p - 2
        z' = z-1
        x = p!!0 + sum [y | j <- [1..(g+1)], let y = p!!j/(z' + fromIntegral j)]
        t = z' + 0.5 + fromIntegral g
        val = sqrt(2*pi) * (t**(z'+0.5)) * exp(-t) * x
        -- p is not recalculated for each call (memoization).
        p = [0.99999999999980993
            , 676.5203681218851
            ,-1259.1392167224028
            , 771.32342877765313
            ,-176.61502916214059
            , 12.507343278686905
            ,-0.13857109526572012
            , 9.9843695780195716e-6
            , 1.5056327351493116e-7
            ]
        
-- Code to read binary data (IEEE754) written by R's writeBin()
-- into a Haskell list. see readrds for an alternative approach.
fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

-- | Reads a list of doubles from a binary file.
readDoubles :: FilePath -> IO [Double]
readDoubles filePath = do
  contents <- BS.readFile filePath
  let len = BS.length contents
      numDoubles = (fromInt64ToInt len) `div` 8
  return $ runGet (getListOfDoubles numDoubles) contents
  where
    getListOfDoubles n = replicateM n getFloat64le

-- | Reads a list of complex doubles from a binary file.
readComplexDoubles :: FilePath -> IO [Complex Double]
readComplexDoubles filePath = do
  contents <- BS.readFile filePath
  let len = BS.length contents
      numComplex = (fromInt64ToInt len) `div` 16
  return $ runGet (getListOfComplexDoubles numComplex) contents
  where
    getListOfComplexDoubles n = replicateM n getComplexDoublele
    getComplexDoublele = do
      r <- getFloat64le
      i <- getFloat64le
      return $ r :+ i

        