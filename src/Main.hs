-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Main
  where

import           Data.Version ( showVersion )
import           System.Console.CmdArgs
import qualified Paths_H

import           H.Module
import           Language.R.Interpreter
import qualified Language.R.Foreign.Internal as R


data Config = Config
    { configFiles :: [String]
    , configGhci  :: Bool
    } deriving (Eq, Data, Typeable, Show)

cmdSpec = Config
  { configFiles = def &= args &= typ "FILES/DIRS"
  , configGhci  = def &= explicit &= name "ghci" &= help "Prepare GHCI compatible output" }
  &=
  verbosity &=
  help "R-to-Haskell translator." &=
  summary ("H version " ++ showVersion Paths_H.version ++
           "\nCopyright (C) 2013 Amgen, Inc.")
  -- TODO: add details clause

main :: IO ()
main = do
    config <- cmdArgs cmdSpec
    case config of
        Config []  _    -> putStrLn "no input files"  -- XXX: exitStatus with fail
        Config [fl] True -> do
            withRInterpret $ \ch -> do
                print =<< parseFile ch fl (\x -> return $ prettyGhci $ translate x (mkMod Nothing "Test"))
        Config fls _    -> withRInterpret $ \ch -> do
            cls <- mapM (\fl -> parseFile ch fl (go fl)) fls
            mapM_ (\(x,y) -> putStrLn (x ++ ":") >> print y) cls
  where
    go fl x = do
        -- R.printValue x    -- TODO: remove or put under verbose
        return (fl, prettyModule $ translate x (mkMod Nothing "Test"))
