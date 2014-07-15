import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Verbosity

import Data.Maybe (fromJust, fromMaybe)
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{ postConf  = hPostConf
                                           , postBuild = hPostBuild
                                           , postCopy  = hPostCopy
                                           }


hPostConf _ flags _ _ 
  | buildMans = do
      mpandoc <- findProgramLocation v "pandoc"
      case mpandoc of
        Nothing -> die "Pandoc is required for building documentation but was not found in your $PATH."
        Just _  -> return ()
  | otherwise = return ()
  where
    v    = fromFlagOrDefault normal $ configVerbosity flags
    buildMans = fromMaybe False $ lookup (FlagName "documentation")
                                $ configConfigurationsFlags flags
 

hPostBuild _ flags _ pkgInfo
  | buildMans = do
      createDirectoryIfMissingVerbose v True pandocPath
      copyFileVerbose v ("doc" </> "pandoc.css") (pandocPath </> "pandoc.css")
    
      pandoc <- fmap fromJust $ findProgramLocation v "pandoc"
      mapM_ (uncurry (runPandoc v pandoc "doc" pandocPath))
            [("H-ints.md","H-ints.html")
            ,("H-user.md","H-user.html")
            ]
  | otherwise = return ()
  where
    v    = fromFlagOrDefault normal $ buildVerbosity flags
    dist = fromFlagOrDefault "dist" $ buildDistPref flags
    pandocPath = dist </> "doc" </> "pandoc"
    buildMans = fromMaybe False $ lookup (FlagName "documentation")
                                $ configConfigurationsFlags
				$ configFlags pkgInfo

hPostCopy _ flags desc pkgInfo
  | buildMans = do
     let dir = docdir $ absoluteInstallDirs desc pkgInfo (fromFlag (copyDest flags))
     installDirectoryContents v  (path) ( dir </> "manual")
  | otherwise = return ()
  where
    v = fromFlagOrDefault normal $ copyVerbosity flags
    dist = fromFlagOrDefault "dist" $ copyDistPref flags
    path = dist </> "doc" </> "pandoc"
    buildMans = fromMaybe False $ lookup (FlagName "documentation")
                                $ configConfigurationsFlags
				$ configFlags pkgInfo 

runPandoc v pandoc docDir toDir input output = do
  notice v $ "Generating documentation file: " ++ output
  rawSystemExit v pandoc ["-f","markdown","--mathjax","-s","-S","--toc","-c","pandoc.css",docDir </> input,"-o",toDir </> output]

