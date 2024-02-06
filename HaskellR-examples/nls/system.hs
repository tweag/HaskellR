import H.Prelude
import Data.Int
import System.Random.MWC
import System.Random.MWC.Distributions

generate :: Int32 -> R s Double
generate x = io $ 
  withSystemRandom . asGenIO $ \gen -> 
    let r = dx*dx+2*dx
    in do v <- standard gen
          return $ r*(1+0.05*v)
  where dx = fromIntegral x

generate_lifted :: [Int32] -> R s [Double]
generate_lifted = mapM generate
