:set -XBangPatterns -XGADTs -XDataKinds
:l system.hs

let next = getLine >> return ()

:{
putStrLn $ unlines 
    [ "Next task will be testing our facilities to inspect values"
    , "As in previous demo we will use haskell functions to create data set:"
    , " H.print =<< [r| xs <- c(1:100) |]"
    , " store <- H.newRVal =<< [r| ys <- generate_lifted_hs(xs) |]"
    ]
:}
next

H.print =<< [r| xs <- c(1:100) |]
store <- H.newRVal =<< [r| ys <- generate_lifted_hs(xs) |]

:{
putStrLn $ unlines 
    [ "Lets discuss it a but now we have used more compicated code"
    , "This is required because this demo uses more memory then previous one"
    , "And R starts garbage collection process"
    , "To protect variables from garbage collection we place them inside protection box"
    , "With API similar to ForeignPtr"
    ]
:}
next

:{
putStrLn $ unlines
  [ "Now we want to aproximate this function by polinomial"
  , "For this purpose we created a Haskell xfunction that creates R expression with formula given"
  , ""
  , "For example:"
  , formula 3 "y" "x"
  , ""
  , "We want to parse this expression by R (using Language.R.parseText) and execute it"
  , "using H.eval"
  , ""
  , "let's introduce haskell function that automate this"
  ]
:}
next

putStrLn "let mkFormula i = eval =<< Language.R.parseText (formula i \"xs\" \"ys\")"
let mkFormula i = eval =<< Language.R.parseText (formula i "ys" "xs") True
 
:{
putStrLn $ unlines [ "Now we may easily inpect our data: (we don't interested in this value so we don't protect it)" 
                   , "val1 <- mkFormula 1"
                   , "H.print val1"
                   ]
:}
val1 <- mkFormula 1
H.print val1

:{
putStrLn $ unlines ["But this approach is not straighforward as we want"
                   , "To analyze haskell value"
                   , "We can access that value by calling:"
                   , "[r| summary(x_hs)$sigma|]"
                   ]
:}
next

putStrLn "val <- (\\x -> [r| summary(x_hs)$sigma |]) =<< mkFormula 1"
val <- (\x -> [r| summary(x_hs)$sigma |]) =<< mkFormula 1
:{
putStrLn $ unlines [ "Here we provide a mechanism to inspect R values"
                   , "its fairy lowlevel and result may be big, but helpful sometimes"
                   , "especially in debugging purposes"
                   ]
:}

putStrLn "putStrLn $ D.inspect val"
putStrLn $ D.inspect val

putStrLn "Sometimes H may be too restricting so we have R.coerce function to 'fix' types"
putStrLn "head $ H.fromSEXP (R.coerce val) :: Double"
head $ H.fromSEXP (R.coerce val) :: Double

:{
putStrLn $ unlines ["now we may want to run different polynomial"
                   , "but now things got more interesting because we allocate much memory"
                   , "r starts garbage collection process"
                   , "so we need to protect all variables"
                   , "h provides rval mechanism with api similar to foreignptr"
                   , "val2 <- h.newrval =<< mkformula 2"
                   , ":{"
                   , "h.withrval val2 $ \\y -> do"
                   , "    h.print y "
                   , "    h.print =<< [r| summary(y_hs)$sigma |]"
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

getLine

-- :{
-- putStrLn $ unlines 
--    [ "generally we are too lazy to write big functions by hands so introducing a function:"
--    , "let go n = do"
--    , "    valN <- H.newRVal =<< mkFormula n"
--    , "    H.withRVal valN $ \\y -> do"
--    , "       H.print y"
--    , "       v <- fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double"
--    , "       Prelude.print v"
--    , "       return valN"
--    ]
-- :}
-- :{
--let go n = do
--    valN <- H.newRVal =<< mkFormula n
--    H.withRVal valN $ \y -> do
--       H.print y 
--       v <- fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
--       Prelude.print v
--       return valN
-- :}
-- val3 <- go 3
-- getLine
-- putStrLn "run it for 4,5,6,7"
-- val4 <- go 4
-- val5 <- go 5
-- val6 <- go 6
-- val7 <- go 7

putStrLn "3:"
val3 <- H.newRVal =<< mkFormula 3
:{
H.withRVal val3 $ \y -> do
     H.print y 
     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
:}
putStrLn "4:"
val4 <- H.newRVal =<< mkFormula 4
:{
H.withRVal val4 $ \y -> do
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
:{
putStrLn $ unlines
    [ "However things may go wrong in R, this may mean that we may caught"
    , "runtime exceptions even if code is correct, for example when method"
    , "doesn't converge."
    , "To prevent such things we may use native haskell exception hangling mechanisms"
    ]
:}
next
putStrLn ":m +Control.Exception"
:m +Control.Exception
putStrLn "6:"
putStrLn "eval6 <- try (H.newRVal =<< mkFormula 6) :: IO (Either SomeException (RVal R.Any))"
eval6 <- try (H.newRVal =<< mkFormula 6) :: IO (Either SomeException (RVal R.Any))
:{
case eval6 of
    Right val6 -> H.withRVal val6 $ \y -> do
                     H.print y 
                     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
    Left e -> Prelude.print e >> return 0
:}
putStrLn "7:"
putStrLn "eval7 <- try (H.newRVal =<< mkFormula 7) :: IO (Either SomeException (RVal R.Any))"
eval7 <- try (H.newRVal =<< mkFormula 7) :: IO (Either SomeException (RVal R.Any))
:{
case eval7 of
    Right val7 -> H.withRVal val7 $ \y -> do
                     H.print y 
                     fmap (head . H.fromSEXP) [r| summary(y_hs)$sigma |] :: IO Double
    Left e -> Prelude.print e >> return 0
:}
getLine

:m +Control.Applicative
putStrLn "now we can analyse it"
H.withRVal val5 $ \y -> do H.print =<< [r| coef(y_hs) |]
cfs <- H.fromSEXP <$> (H.withRVal val5 $ \y -> [r| coef(y_hs) |]) :: IO [Double]
 
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
          case eval6 of
            Right val6 ->
              H.withRVal val6 $ \v6 ->
                [r| plot(xs,ys) 
                    lines(xs,predict(v2_hs), col = 2)
                    lines(xs,predict(v3_hs), col = 3)
                    lines(xs,predict(v4_hs), col = 4)
                    lines(xs,predict(v5_hs), col = 5)
                    lines(xs,predict(v6_hs), col = 6)
                |]
            Left _ ->
                [r| plot(xs,ys) 
                    lines(xs,predict(v2_hs), col = 2)
                    lines(xs,predict(v3_hs), col = 3)
                    lines(xs,predict(v4_hs), col = 4)
                    lines(xs,predict(v5_hs), col = 5)
                |]
:}
