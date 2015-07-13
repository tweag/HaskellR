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
                                           , postInst  = hPostInst
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

copyManual verbosityFlag cd distPrefFlag desc buildInfo
  | shouldBuildManuals buildInfo = do
     let destdir = docdir $ absoluteInstallDirs desc buildInfo cd
         dist = fromFlagOrDefault "dist" distPrefFlag
     installDirectoryContents v (dist </> "doc" </> "pandoc") ( destdir </> "manual")
  | otherwise = return ()
  where
    v = fromFlagOrDefault normal verbosityFlag

hPostCopy _ flags = copyManual (copyVerbosity flags) (fromFlag (copyDest flags)) (copyDistPref flags)

hPostInst _ flags = copyManual (installVerbosity flags) NoCopyDest (installDistPref flags)
