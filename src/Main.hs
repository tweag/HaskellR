-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Exception
import           Control.Monad ( void )
import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import           System.Process
import           System.Environment

import qualified Paths_H
#ifdef H_ARCH_UNIX
import           System.Posix.Signals
#endif

data H = H
    { configFiles :: [FilePath] 
    , configInteractive  :: FilePath
    , configInteractiveQQ :: String
    } deriving (Eq, Data, Typeable, Show)

cmdSpec :: H
cmdSpec = H
  { configFiles = def &= args  &= typ "ghci options" 
  , configInteractive  = "ghci" &= explicit &= name "interactive" &= help "Run interpreter" &= opt "ghci" &= typ "ghci" &= help "Set an alternative haskell interpreter." 
  , configInteractiveQQ = "default" &= explicit &= name "interactive-qq" &= opt "default" &= help "set quasiquoter engine for debug reasons. Possible options: runtime, default"
  }
  &= program "H" &=
  help "H wrapper over ghci. " &=
  summary ("H version " ++ showVersion Paths_H.version ++
           "\nCopyright (C) 2013-2014 Amgen, Inc.")
  -- TODO: add details clause

main :: IO ()
main = do
    config <- cmdArgs cmdSpec
    case config of
      H {configFiles, configInteractive, configInteractiveQQ} -> do
        cfg  <- Paths_H.getDataFileName "H.ghci"
        env' <- fmap (\e -> case configInteractiveQQ of
                              "default" -> e
                              "runtime" -> ("H_INTERACTIVE_RUNTIME_QQ", "1"):e
                              _         -> error "runtime type can be one of the: default, runtime"
                      ) getEnvironment
        let argv = configFiles ++ ["-v0", "-ghci-script", cfg]
#if MIN_VERSION_process(1,2,0)
#ifdef H_ARCH_UNIX
        _ <- installHandler sigINT Ignore Nothing
        _ <- installHandler sigTERM Ignore Nothing
        _ <- installHandler sigQUIT Ignore Nothing
#endif
        (_,_,_,ph) <-
            createProcess (proc configInteractive argv)
            { std_in = Inherit
            , std_out = Inherit
            , delegate_ctlc = False
            , env = Just env'
            }
#else
#ifdef H_ARCH_UNIX
        _ <- installHandler sigINT Ignore Nothing
        _ <- installHandler sigTERM Ignore Nothing
        _ <- installHandler sigQUIT Ignore Nothing
#endif
        (_,_,_,ph) <-
            createProcess (proc configInteractive argv)
            { std_in = Inherit
            , std_out = Inherit
            }
#endif
        let loop = (void $ waitForProcess ph) `onException` (putStrLn "exception" >> loop)
        loop
        putStrLn "Bye!"
