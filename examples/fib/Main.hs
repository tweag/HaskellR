{-# LANGUAGE QuasiQuotes #-}
module Main
  where

import Fib

import qualified H.Prelude as H
import           Language.R.QQ
import qualified Language.R
import qualified Language.R.Instance
import qualified Foreign.R
import qualified Foreign.R.Interface
import           Control.Monad.IO.Class
      ( liftIO  )

main :: IO ()
main = do
    Language.R.pokeRVariables
        ( Foreign.R.globalEnv
        , Foreign.R.baseEnv
        , Foreign.R.nilValue
        , Foreign.R.unboundValue
        , Foreign.R.missingArg
        , Foreign.R.rInteractive
        , Foreign.R.Interface.rCStackLimit
        , Foreign.R.rInputHandlers
        )
    H.runR Language.R.Instance.defaultConfig $ do
        H.print =<< [r| "test" |]
        H.print =<< [r| 1+2 |]
        liftIO $ putStrLn "[r| neg_hs(TRUE, as.integer(5)) |]"
        H.print =<< [r| neg_hs(TRUE, as.integer(5)) |]
        liftIO $ putStrLn "[r| neg_hs(FALSE, as.integer(6)) |]"
        H.print =<< [r| neg_hs(FALSE, as.integer(6)) |]
        liftIO $ putStrLn "[r| neg_hs(NA, as.integer(7)) |]"
        H.print =<< [r| neg_hs(NA, as.integer(7)) |]
        liftIO $ putStrLn "[r| fib_hs(as.integer(1)) |]"
        H.print =<< [r| fib_hs(as.integer(1)) |]
        liftIO $ putStrLn "[r| fib_hs(as.integer(10)) |]"
        H.print =<< [r| fib_hs(as.integer(10)) |]
        liftIO $ putStrLn "[r| fact_hs(as.integer(0)) |]"
        H.print =<< [r| fact_hs(as.integer(0)) |]
        liftIO $ putStrLn "[r| fact_hs(as.integer(7)) |]"
        H.print =<< [r| fact_hs(as.integer(7)) |]
        liftIO $ putStrLn "[r| factSexp_hs(as.integer(7)) |]"
        H.print =<< [r| factSexp_hs(as.integer(7)) |]

