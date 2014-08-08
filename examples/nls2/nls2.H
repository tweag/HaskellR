:set -XBangPatterns -XGADTs -XDataKinds
:l system.hs
:m +Control.Memory.Region

let next = getLine >> return ()

:{
putStrLn $ unlines 
    [ "Next task will be testing our facilities to inspect values"
    , "As in previous demo we will use haskell functions to create data set:"
    , " H.print =<< [r| xs <- c(1:100) |]"
    , " store <- [r| ys <- generate_lifted_hs(xs) |]"
    ]
:}
next

rx <- [r| xs <- c(1:100) |]
store <- [r| ys <- generate_lifted_hs(xs) |]

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
R.unSomeSEXP val (putStrLn . D.inspect)

putStrLn "Sometimes H may be too restricting so we have R.coerce function to 'fix' types"
putStrLn "head $ H.fromSEXP (R.coerce val) :: Double"
R.unSomeSEXP val (head . H.fromSEXP) :: Double

:{
putStrLn $ unlines ["now we may want to run different polynomial"
                   , "but now things got more interesting because we allocate much memory"
                   , "r starts garbage collection process"
                   , "so we need to protect all variables"
                   ]
:}

putStrLn "2:"
val2 <- mkFormula 2
H.print val2
z <- [r| summary(val2_hs)$sigma |]
R.unSomeSEXP z (head . H.fromSEXP) :: Double

getLine

putStrLn "3:"
val3 <- mkFormula 3
H.print val3
z <- [r| summary(val3_hs)$sigma |]
R.unSomeSEXP z (head . H.fromSEXP) :: Double

putStrLn "4:"
val4 <- mkFormula 4
H.print val4
z <- [r| summary(val4_hs)$sigma |]
R.unSomeSEXP z (head . H.fromSEXP) :: Double

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
putStrLn "5:"
putStrLn "eval5 <- try (H.newRVal =<< mkFormula 5) :: IO (Either SomeException (RVal R.Any))"
eval5 <- try (mkFormula 5) :: IO (Either SomeException (SomeSEXP G))
:{
case eval5 of
    Right val5 -> do H.print val5
                     z <- [r| summary(val5_hs)$sigma |]
                     return $ R.unSomeSEXP z (head . H.fromSEXP) :: IO Double
    Left e -> Prelude.print e >> return 0
:}
putStrLn "6:"
putStrLn "eval6 <- try (H.newRVal =<< mkFormula 6) :: IO (Either SomeException (RVal Foreign.R.Any))"
eval6 <- try (mkFormula 6) :: IO (Either SomeException (SomeSEXP G))
:{
case eval6 of
    Right val6 -> do H.print val6
                     z <- [r| summary(val6_hs)$sigma |]
                     return $ R.unSomeSEXP z (head . H.fromSEXP) :: IO Double
    Left e -> Prelude.print e >> return 0
:}
putStrLn "7:"
putStrLn "eval7 <- try (H.newRVal =<< mkFormula 7) :: IO (Either SomeException (RVal Foreign.R.Any))"
eval7 <- try (mkFormula 7) :: IO (Either SomeException (SomeSEXP G))
:{
case eval7 of
    Right val7 -> do H.print val7
                     z <- [r| summary(val7_hs)$sigma |]
                     return $ R.unSomeSEXP z (head . H.fromSEXP) :: IO Double
    Left e -> Prelude.print e >> return 0
:}
getLine
 
:m +Control.Applicative
:{
let cval = case eval7 of
            Right x -> x
            Left _ -> case eval6 of
              Right x -> x
              Left _ -> case eval5 of
                Right x -> x
                Left _ -> val4
:}
putStrLn "now we can analyse it"
H.print =<< [r| coef(cval_hs) |]
z <- [r| coef(cval_hs) |]
let cfs = R.unSomeSEXP z (H.fromSEXP) :: [Double]

let fn x = sum $ zipWith (*) cfs (iterate (*x) 1)
putStrLn "value in a point 7"
fn 7
map fn [1..100]

:m +Control.Monad
let isRight ex = case ex of { Right x -> True ; _ -> False}
putStrLn "plots"

:{
_ <- [r| plot(rx_hs,ys) 
         lines(rx_hs,predict(val2_hs), col = 2)
         lines(rx_hs,predict(val3_hs), col = 3)
         lines(rx_hs,predict(val4_hs), col = 4) |]
:}

:{
case eval5 of
  Right val5 -> void $ [r| lines(rx_hs,predict(val5_hs), col = 5) |]
  _ -> return ()
:}
