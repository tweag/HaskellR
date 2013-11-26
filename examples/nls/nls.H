-- Initializing R runtime and all constants
-- Copyright: (C) 2013 Amgen, Inc.
:m +Control.Monad
:m +Foreign.R
:l system.hs
let next = putStrLn "<Press enter to continue>" >> void (getLine)
:{
putStrLn $
    unlines [ "In this demo we will run H using haskell callbacks"
            , "and discuss some issues with current implementation"
            , "and possibilities."
            , ""
            , "Files that are used:"
            , "   * nls.H - file with script itself"
            , "   * .ghci - ghci config file, will be integrated soon"
            , "   * system.hs - haskell module that is used in callbacks"
            , ""
            , "How to run this example:"
            , "   ghci -v0 -ghci-script nls.H"
            ]
:}
next
:{
putStrLn $
  unlines [ "Prelare points in R:"
          , "We are creating 'xs' points with R command [r| xs <- c(1:100) |]"
          , "   [r| ... |] command creates a command that may be evaluated in GHCi"
          , "              or used as an R expression later"
          , ""
          , "To evaluate command you may use one of the following functions:"
          , "   H.Prelude.evalIO - strictly evaluates expression in IO Monad"
          , "   H.Prelude.eval_  - evaluate expression in IO Monad discarding result"
          , "                      (no output will be printed)"
          , "   H.Prelude.eval   - pure lazy evaluation of the expression."
          , ""
          , "To print result one may use R facilities: "
          , "   printValue       - prints expression like R does"
          , ""
          , "Thus we are calling: printValue $ eval [r| xs <- c(1:100) |]"
          ]
:}
next
putStrLn "printValue [r| xs <- c(1:100) |] - expression"
printValue [r| xs <- c(1:100) |]
putStrLn "printValue $ eval [r| xs <- c(1:100) |] - result"
printValue $ eval [r| xs <- c(1:100) |]
next
:{
putStrLn $ 
  unlines [ "Now for each point we want to calculate complicated function"
          , "of cause if example we will use simple example"
          , "But we wil use separate file for it"
          , "We are generating function x^2+2*x+5 with some random noise"
          , "with standart distribution (using mwc-random package)."
          , "Print 10 examples for x=10:"
          ]
:}
replicateM 10 (generate 10)
next
:{
putStrLn $ 
  unlines [ "Now we want to use that function in R:"
          , "in order to use function we need to lift it on vector level"
          ]
:}
putStrLn "eval_ [r| ys <- (function(x).Call(generate_lifted_hs,x))(xs) |]"
eval_ [r| ys <- (function(x).Call(generate_lifted_hs,x))(xs) |]
next
putStrLn "eval [r| nlmod <- nls( ys~a*xs*xs+b*xs+c, start=list( a = 0.13, b = 1.5, c = 0.4)) |]"
printValue $ eval [r| nlmod <- nls( ys~a*xs*xs+b*xs+c, start=list( a = 0.13, b = 1.5, c = 0.4)) |]
next
eval_ [r| plot(xs,ys,main="nls") |]
eval_ [r| lines(xs,predict(nlmod), col = 2)|]
getLine
