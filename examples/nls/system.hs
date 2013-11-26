import System.Random.MWC
import System.Random.MWC.Distributions

generate :: Double -> IO Double
generate x =
  withSystemRandom . asGenIO $ \gen -> 
    let r = x*x+2*x
    in do v <- standard gen
          return $ r*(1+0.10*v)

generate_lifted :: [Double] -> IO [Double]
generate_lifted = mapM generate
