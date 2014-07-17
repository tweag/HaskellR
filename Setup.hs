import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Verbosity

import Data.Maybe (fromMaybe)
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{ postBuild = hPostBuild
                                           , postCopy  = hPostCopy
                                           }

shouldBuildManuals :: LocalBuildInfo -> Bool
shouldBuildManuals =
    fromMaybe False .
    lookup (FlagName "documentation") .
    configConfigurationsFlags .
    configFlags

hPostBuild _ flags _ buildInfo
  | shouldBuildManuals buildInfo = do
      rawSystemExit v "make" ["doc-manuals"]
  | otherwise = return ()
  where
    v = fromFlagOrDefault normal $ buildVerbosity flags

hPostCopy _ flags desc buildInfo
  | shouldBuildManuals buildInfo = do
     let destdir = docdir $ absoluteInstallDirs desc buildInfo (fromFlag (copyDest flags))
         dist = fromFlagOrDefault "dist" $ copyDistPref flags
     installDirectoryContents v (dist </> "doc" </> "pandoc") ( destdir </> "manual")
  | otherwise = return ()
  where
    v = fromFlagOrDefault normal $ copyVerbosity flags
