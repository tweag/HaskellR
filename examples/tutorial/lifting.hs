{-# LANGUAGE QuasiQuotes, DataKinds #-}
module Main where
{-

This file extends an example found on the HaskellR documenttaion page:
https://tweag.github.io/HaskellR/docs/differences-repl-source.html,
an example where Language.R.Instance.Interactive is not used (like
it is in H or Jupyter). It is suggested that in .hs files, the R
monad should be used instead of the IO monad. This provides stronger
static guarantees than the IO monad does.

In contrast, tutorial.hs makes heavy use of the IO
monad to keep the code simple, and to permit easier IO, without
the need for lifting.

Here the type 'R s ()' is used in place of 'IO ()'. The latter is
an instance of MonadIO, so liftIO can be used for user interaction
if needed.

For a batch program that does not need to interact with the user,
runRegion should be used once, after withEmbeddedR.

For an interactive program, each subtask/function should be in
a runRegion block to be sure it is fully evaluated (no unevaluated
thunks).

The function p defined below is used instead of the interactive
function I.p in tutorial.hs, and the function phelp is a lifted
version of the function used to insert help comments.

-}
  
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import qualified Language.R as R

import System.IO
import System.Exit

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Int(Int32)
import qualified Data.Vector.SEXP as V
import qualified Data.Vector.SEXP.Mutable as M

-- Convenience types for working with proxy vectors
type RProxyVectorDouble = V.Vector 'R.Real Double
type RProxyVectorInt    = V.Vector 'R.Int  Int32

-- print result of a quasiquote (R does the printing)
p x = [r| print(x_hs) |] >> return ()

-- print a list of strings, one per line (help text)
phelp = liftIO . putStrLn . unlines :: [String] -> R s ()

example1 :: String -> R s ()
example1 txt = do
  phelp [txt]
  liftIO $ putStrLn "Plotting sine and cosine."
  p =<< [r| plot(0:200/20, sin(0:200/20),
           xlab='t',ylab='signal',type='l',col='blue',
           main="Orig. signal blue, Hilbert transform red")
           lines(0:200/20, cos(0:200/20),type='l',col='red') 
           dev.flush() |]
  
example2 :: String -> R s ()
example2 txt = do
  phelp [ txt
        , "Function and gradient are defined in Haskell"
        ]
  let fn x y = return ((x - 1.0)^2 + (y - 2.0)^2) :: R s Double
      gr x y = return ([2*(x - 1.0), 2*(y - 2.0)]) :: R s [Double]
      startXY = [0.0, 0.0] :: [Double] -- initial guess
  liftIO $ putStrLn "Using R's optimizer..."
  p =<< [r| library(optimx)
          fn <- function(v) fn_hs(v[1], v[2])
          gr <- function(v) gr_hs(v[1], v[2])      
          optimx(startXY_hs, fn, gr, control=list(reltol=1e-12)) |]
  
example3 :: String -> R s ()
example3 txt = do
  phelp [txt]
  let vd = V.fromList [1..5] :: RProxyVectorDouble
  p =<< [r| vd_hs |]

-- Database of user choices.
db = [ (1,"Plot sine and cosine curves")
     , (2,"Use R's optimizer")
     , (3,"Create proxy vector and display using R")
     ]  

showItem :: (Int,String) -> String
showItem (k,s) = (show k)++": "++s

-- The trick used in tutorial.hs to invoke the example functions
-- does not work here, because the case
-- Just (a,b) -> R.runRegion do b
-- leads to a type mismatch: type variable 's' in R s () would
-- escape its scope. Another complication here is that the
-- examples need not have the same signature.
exampleProcessor :: IO ()
exampleProcessor = do
  putStrLn ""
  mapM_ (putStrLn . showItem) db
  putStr "\nTut> "
  hFlush stdout
  line <- getLine
  when(line /= "") $ do
    let result = read line::Int
    case result of
      1 -> R.runRegion $ do
             example1 $ snd $ db!!0
      2 -> R.runRegion $ do
             example2 $ snd $ db!!1
      3 -> R.runRegion $ do
             example3 $ snd $ db!!2
      _ -> exitSuccess
    exampleProcessor

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
  exampleProcessor
