:set -XBangPatterns -XGADTs
:l system.hs

let next = getLine >> return ()

:{
putStrLn $ unlines 
    [ "Next task will be testing our facilities to inspect values"
    , "As in previous demo we will use haskell functions to create data set"
    ]
:}
next

H.print =<< [r| xs <- c(1:100) |]
store <- H.newRVal =<< [r| ys <- (function(x).Call(generate_lifted_hs,x))(xs) |]
:{
putStrLn $ unlines
  [ "Now we want to aproximate this function by polinomial"
  , "For this purpose we created a function that creates R expression with formula given"
  , "For example:"
  , formula 3 "x" "y"
  , "We want to parse this expression by R and execute it, let's introduce haskell function"
  , "for this"
  ]
:}
next


putStrLn "let mkFormula i = H.eval =<< Language.R.parseText (formula i \"xs\" \"ys\")"
let mkFormula i = eval =<< Language.R.parseText (formula i "ys" "xs")

putStrLn $ unlines [ "Now we may easily inpect our data" ]
-- val1 <- mkFormula 1
-- H.print val1

:{
putStrLn $ unlines ["But this approach is not straighforward as we want"
                   , "To analyze haskell value"
                   , "We can access that value by calling:"
                   , "[r| summary(x_hs)$sigma|]"
                   , "Here we provide a mechanism to inspect R values"
                   , "its fairy lowlevel and result may be big, but helpful sometimes"
                   ]
:}
next

val <- (\x -> [r| summary(x_hs)$sigma |]) =<< mkFormula 1
putStrLn $ D.inspect val
head $ H.fromSEXP (R.coerce val) :: Double

:{
putStrLn $ unlines ["Now we may want to run different polynomial"
                   , "But now things got more interesting because we allocate much memory"
                   , "R starts garbage collection process"
                   , "So we need to protect all variables"
                   , "H provides RVal mechanism with API similar to ForeignPtr"
                   , "val2 <- H.newRVal =<< mkFormula 2"
                   , ":{"
                   , "H.withRVal val2 $ \\y -> do"
                   , "    H.print y "
                   , "    H.print =<< [r| summary(y_hs)$sigma |]"
                   , ":}"
                   ]
:}

putStrLn "2:"
val2 <- H.newRVal =<< mkFormula 2
:{
H.withRVal val2 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "new plot"
getLine
putStrLn "3:"
val3 <- H.newRVal =<< mkFormula 3
:{
H.withRVal val2 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "4:"
val4 <- H.newRVal =<< mkFormula 4
:{
H.withRVal val2 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "5:"
val5 <- H.newRVal =<< mkFormula 5
:{
H.withRVal val5 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "6:"
val6 <- H.newRVal =<< mkFormula 6
:{
H.withRVal val6 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "7:"
val7 <- H.newRVal =<< mkFormula 7

-- :{
-- H.withRVal val7 $ \y -> do
--      H.print y 
--      fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
-- :}
-- getLine

:m +Control.Applicative
putStrLn "now we can analyse it"
H.withRVal val6 $ \y -> do H.print =<< [r| coef(y_hs) |]
cfs <- H.fromSEXP <$> (H.withRVal val6 $ \y -> [r| coef(y_hs) |]) :: IO [Double]
 
putStrLn "create a function from coeffs"
let fn x = sum $ zipWith (*) cfs (iterate (*x) 1)
putStrLn "value in a point 7"
fn 7
map fn [1..100]

putStrLn "calculate difference (x-y)^2"
H.withRVal store $ \yhs -> return $ sum $ zipWith (\a b -> (a-b)*(a-b)) (H.fromSEXP (R.coerce yhs)) (map  fn [1..100])

putStrLn "plots"
:{
  H.withRVal val2 $ \v2 ->
    H.withRVal val3 $ \v3 ->
      H.withRVal val4 $ \v4 ->
        H.withRVal val5 $ \v5 ->
          H.withRVal val6 $ \v6 ->
            [r| plot(xs,ys) 
                lines(xs,predict(v2_hs), col = 2)
                lines(xs,predict(v3_hs), col = 3)
                lines(xs,predict(v4_hs), col = 4)
                lines(xs,predict(v5_hs), col = 5)
                lines(xs,predict(v6_hs), col = 5)
            |]
:}
