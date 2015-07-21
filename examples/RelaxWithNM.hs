{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module    : Main
--
-- Module originally contributed by Dominic Steinitz. Modifications
-- made by Tweag I/O (2015).

module Main where

import             Control.Applicative
import             Control.DeepSeq
import             Control.Monad
import             Data.Int
import qualified   Foreign.R as R
import qualified   Foreign.R.Type as R
import             H.Prelude as H
import             Language.R.QQ
import             Numeric.Integration.TanhSinh
import "temporary" System.IO.Temp (withSystemTempDirectory)

safeHead :: String -> [a] -> a
safeHead msg [] = error $ "You have erred: " ++ msg
safeHead _   xs = head xs

maxRelaxIters :: Int
maxRelaxIters = 100

forcingFunction :: Double -> Double
forcingFunction = (\t -> bigA * sin (omega * t))
  where
    omega = 1.0
    bigA  = 1.0

(.*.) :: (Double -> Double)
         -> (Double -> Double) -> Double -> Double
f .*. g = h
  where
    h t = result $ absolute 1e-6 $ parTrap (\x -> (f x) * (g (t - x))) 0 t

flow :: (Double -> Double)
        -> Double -> Double -> Double -> Double -> Double
flow fn a initVal initT = \t -> term1 (t - initT) + term2 (t - initT)
  where
    term1 = \t -> initVal * exp (a * t)
    term2 = (fn . (+ initT)) .*. (\t -> exp (a * t))

timeStep :: Double
timeStep = 0.1

within6digits :: (Ord a, Fractional a) => a -> a -> Bool
within6digits x y = abs (x - y) < 1e-06

within6digitss :: (Ord b, Fractional b) =>
                  [b] -> [b] -> Bool
within6digitss xs ys = and (zipWith within6digits xs ys)

lerp :: Num a => a -> a -> a -> a
lerp y1 y2 x = y1 * (1 - x) + y2 * x

linearly :: Fractional s => (s, s) -> (s, s) -> s -> s
linearly (x1,y1) (x2,y2) = \x -> lerp y1 y2 $ (x - x1) / (x2 - x1)

linearlyL :: Fractional c =>
             (c, [c]) -> (c, [c]) -> c -> [c]
linearlyL (t1, x1s) (t2, x2s) t =
  zipWith (\x1 x2 -> linearly (t1, x1) (t2, x2) t) x1s x2s

relax :: (t2 -> t3 -> t4 -> t -> t -> t4)
      -> (t4 -> t1)
      -> ((t, t1) -> (t, t1) -> t3)
      -> t2
      -> (Int, t4 -> t4 -> Bool, [t], t4)
      -> [(t, t4)]
relax _    _    _           _         (_            ,_          ,[]   ,_ ) = []
relax _    _    _           _         (maxRelaxIters',_          ,_    ,_ )
  | maxRelaxIters' <= 0                                                     = []
relax scrF scrG interpolate forcingFn (_maxRelaxIters,closeEnough,t0:ts,a0) = as

  where

    as = scanl outer (t0,a0) ts

    outer (t1,a1) t2 = (t2, betterGuess)
      where

        betterGuess = fst $
                      safeHead "Use 1" $
                      dropWhile (not . uncurry closeEnough) $
                      take maxRelaxIters $
                      zip betters (tail betters)

        betters = iterate better a1

        better a2 = scrF forcingFn interpFn a1 t1 t2
          where
            interpFn = interpolate (t1, scrG a1) (t2, scrG a2)

solve :: [(Double, Double)] -> [(Double, [Double])]
solve initsGains =
  relax scrF scrG linearlyL forcingFunction
        (maxRelaxIters, within6digitss, (map (timeStep*) [0..199]), inits)
  where
    inits = map fst initsGains
    gains = map snd initsGains
    scrG = id

    scrF _  _   []     _  _  = []
    scrF forcingFn bubbleFns (a:bs) t0 t1 = aResult : results
      where
        bFLast t = last (bubbleFns t)
        aResult  = flow (liftM2 (+) forcingFn bFLast) (safeHead "Use 2" gains) a t0 t1
        l = length gains - 1
        tailGains =  tail gains
        ns        =  [0..l - 1]
        f n gain initVal =
          flow (\t -> (bubbleFns t)!!n) gain initVal t0 t1
        results = zipWith3 f ns tailGains bs

initsParams :: [(Double, Double)]
initsParams =
  [ (2.0, -2.0)
  , (2.0/3.0, -1.0/6.0)
  , (4.0, -3.0)
  ]

dataPoints :: [Double]
dataPoints = map (!!1) $ map snd $ solve initsParams

forFitting :: Double -> Double -> Double -> [Double]
forFitting alpha beta gamma = map (!!1) $ map snd $ solve ips
  where
    ips = zip (map fst initsParams) [alpha, beta, gamma]

-- | Cost function
cost :: Int -> Double -> Double -> Double -> Double
cost n alpha beta gamma =
  sum $
  map (^ (2 :: Integer)) $
  take n $
  zipWith (-) dataPoints (forFitting alpha beta gamma)


nmMin :: Int -> IO (Double, Double, Double, Double, Double, Int32, Int32)
nmMin n = runRegion $ do
    initParms <- [r| c(-1.9,-0.1,-2.9) |]
    initVal  <- H.fromSEXP . R.cast R.SReal <$> [r|(function(v) costH_hs(v[1], v[2], v[3]))(initParms_hs)|]
    relaxMin <- [r| optimx(c(-1.9,-0.1,-2.9), function(v) costH_hs(v[1], v[2], v[3]), method = "Nelder-Mead") |]
    aMin     <- H.fromSEXP . R.cast R.SReal <$> [r| relaxMin_hs$p1 |]
    bMin     <- H.fromSEXP . R.cast R.SReal <$> [r| relaxMin_hs$p2 |]
    cMin     <- H.fromSEXP . R.cast R.SReal <$> [r| relaxMin_hs$p3 |]
    vMin     <- H.fromSEXP . R.cast R.SReal <$> [r| relaxMin_hs$value |]
    fEvals   <- H.fromSEXP . R.cast R.SInt  <$> [r| as.integer(relaxMin_hs$fevals) |]
    convCode <- H.fromSEXP . R.cast R.SInt  <$> [r| as.integer(relaxMin_hs$convcode) |]
    return $!! (initVal,aMin,bMin,cMin,vMin,fEvals,convCode)
  where
    costH :: Double -> Double -> Double -> R s Double
    costH a b c = return $ cost n a b c


main :: IO ()
main = withEmbeddedR defaultConfig $
       withSystemTempDirectory "RelaxWithNM_R" $ \dest -> do
  runRegion $ do
    _ <- [r| install.packages(c("numDeriv", "optimx"), lib = dest_hs, repos = "http://cran.us.r-project.org") |]
    _ <- [r| library('numDeriv', lib.loc = dest_hs) |]
    _ <- [r| library('optimx', lib.loc = dest_hs) |]
    return ()
  results <- mapM nmMin [100..102]
  putStrLn $ Prelude.show results
