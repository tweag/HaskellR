-- Initializing R runtime and all constants
-- Copyright: (C) 2013 Amgen, Inc.
:set -fno-ghci-sandbox
:set -XQuasiQuotes
:set -XTemplateHaskell
:m +H.Prelude
:m +Language.R.Runtime.QQ
:{
:def initR
     (\_ -> return
        $ ":cmd Language.R.Interpreter.initializeR Nothing "
       ++ ">> (Data.IORef.writeIORef Language.R.globalEnv =<< Foreign.peek Foreign.R.globalEnv) "
       ++ ">> (Data.IORef.writeIORef Language.R.globalEnv =<< Foreign.peek Foreign.R.globalEnv) "
       ++ ">> (Data.IORef.writeIORef Language.R.nilValue =<< Foreign.peek Foreign.R.nilValue) "
       ++ ">> (Data.IORef.writeIORef Language.R.unboundValue =<< Foreign.peek Foreign.R.unboundValue) "
       ++ ">> (Data.IORef.writeIORef Language.R.baseEnv =<< Foreign.peek Foreign.R.baseEnv) "
       ++ ">> (Foreign.poke Foreign.R.rInteractive 0) "
       ++ ">> return \"\""
     )
:}
:m +Control.Monad
:l system.hs
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
putStrLn "Initializing R (we may integrate this command inside H)"
:initR
:{
putStrLn $
  unlines [ "Prelare points in R:"
          , "We are creating 'xs' points with R command [r| xs <- c(1:100) |]"
          , "This command will create an expression that can be executed by H.Prelude.eval"
          , "You may use printValue R command to print result"
          , "Thus we are calling: printValue $ eval [r| xs <- c(1:100) |]"
          ]
:}
:m +Foreign.R
printValue $ eval [r| xs <- c(1:100) |]
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
:{
putStrLn $ 
  unlines [ "Now we want to use that function in R:"
          , "for this we will generate R table."
          , "<Now you can see additional output but it could be fixed>"]
:}
eval [r| ys <- (function(x).Call(generate_lifted_hs,x))(xs) |]
printValue $ eval [r| nlmod <- nls( ys~a*xs*xs+b*xs+c, start=list( a = 0.13, b = 2, c = 0.4)) |]
eval [r| plot(xs,ys,main="nls") |]
eval [r| lines(xs,predict(nlmod), col = 2)|]
getLine
