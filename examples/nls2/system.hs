{-# LANGUAGE ViewPatterns, GADTs #-}
import H.Prelude
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified Foreign.R as R
import Language.R.HExp
import qualified Data.Vector.SEXP as Vector
import Language.R.Debug as D
import Data.List
import Data.Int

generate :: Int32 -> IO Double
generate ix =
  withSystemRandom . asGenIO $ \gen -> 
    -- Lets make some more interesting distribution:
    let r = (x-10)*(x-20)*(x-40)*(x-70)
          + 28*x*(log x)
    in do v <- standard gen
          return $ r * (1 + 0.01 * v)
  where x = fromIntegral ix

generate_lifted :: [Int32] -> R s [Double]
generate_lifted = io .  (mapM generate)

data Poly = Poly [Int]

generate_polynomial :: Int -> String -> String
generate_polynomial 0 s = "a0"
generate_polynomial x s = "a" ++ Prelude.show x ++ "*" ++ intercalate "*" (replicate x s) ++ "+" ++ generate_polynomial (x-1) s

generate_list :: Int -> String
generate_list n = -- intercalate "," $ zipWith (\a b -> a++"="++show b) (map (\t->"a"++show t) [0..(n+1)]) (reverse lst)
    intercalate "," $ map (\i -> "a" ++ Prelude.show i ++ "=1") [0..n]
  where
    lst = [0.13, 1.5, 0.4, 19, 27, 7, 9 ]
--    lst = [5600,1060,630,140,10,50,1]

formula :: Int -> String -> String -> String
formula n y x = "nls( "++y++" ~ "++generate_polynomial n x++", start=list("++generate_list n++"))"

