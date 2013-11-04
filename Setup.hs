import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

main = defaultMainWithHooks simpleUserHooks{ preBuild = genEnvironment }

genEnvironment _ args = do
    putStrLn "Generating dist/build/autogen/Env_H"            -- TODO: check vebosity
    createDirectoryIfMissing True "dist/build/autogen/"
    -- TODO: drop dependency on the process library
    -- TODO: read pkg-config from info
    pkg <- fmap (head.lines) (readProcess "pkg-config" ["--variable=rhome","libR"] "")
    writeFile "dist/build/autogen/Env_H.hs" $ unlines
      [ "module Env_H where"
      , ""
      , "envRHOME :: String"
      , "envRHOME = \""++pkg++"\""
      ]
    return emptyHookedBuildInfo
