-- Initializing R runtime and all constants
-- Copyright: (C) 2013 Amgen, Inc.
:m +Control.Monad
:set -XQuasiQuotes
:load system.hs
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
            , "   H -- -ghci-script H/H.ghci -ghci-script nls.H"
            , "You will need mwc-random package to be installed"
            ]
:}
next
:{
putStrLn $
  unlines [ "Prelare points in R:"
          , "We are creating 'xs' points with R command [r| xs <- c(1:100) |]"
          , "   [r| ... |] creates expression and evaluates it"
          , ""
          , "To evaluate command you may use one of the following functions:"
          , "   H.evalIO - strictly evaluates expression in IO Monad"
          , "   H.eval_  - evaluate expression in IO Monad discarding result"
          , "                      (no output will be printed)"
          , "   H.eval   - pure lazy evaluation of the expression."
          , ""
          , "To print result one may use R facilities: "
          , "   printR       - prints expression like R does"
          , ""
          , "Thus we are calling: H.print [r| xs <<- c(1:100) |]"
          ]
:}
next
putStrLn "printValue $ eval [r| xs <<- c(1:100) |] - result"
printR =<< [r| xs <<- c(1:100) |]
next
:{
putStrLn $
  unlines [ "Now for each point we want to calculate complicated function"
          , "of cause if example we will use simple example"
          , "But we wil use separate file for it"
          , "We are generating function x^2+2*x+5 with some random noise"
          , "with standart distribution (using mwc-random package)."
          ]
:}
next
:{
putStrLn $
  unlines [ "Now we want to use that function in R:"
          , "in order to use function we need to lift it on vector level"
          ]
:}
putStrLn "[r| ys <<- generate_lifted_hs(xs) |]"
printR =<< [r| ys <<- generate_lifted_hs(xs) |]
next
putStrLn "[r| nlmod <<- nls( ys~a*xs*xs+b*xs+c, start=list( a = 0.13, b = 1.5, c = 0.4)) |]"
printR  =<< [r| nlmod <<- nls( ys~a*xs*xs+b*xs+c, start=list( a = 0.13, b = 1.5, c = 0.4)) |]
next
[r| plot(xs,ys,main="nls") |]
[r| lines(xs,predict(nlmod), col = 2)|]
getLine
