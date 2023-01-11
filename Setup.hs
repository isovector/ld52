import Data.Maybe
import Distribution.AppImage
import Distribution.Simple
import Distribution.Simple.Program.Find
import Distribution.Simple.Setup
import Distribution.Verbosity
import System.Directory.Recursive

main :: IO ()
main = do
  has_appimage <- isJust <$> findProgramOnSearchPath normal defaultProgramSearchPath "appimagetool"
  has_linuxdeploy <- isJust <$> findProgramOnSearchPath normal defaultProgramSearchPath "linuxdeploy"
  if (has_appimage && has_linuxdeploy)
    then do
      res <- getFilesRecursive "./resources"
      defaultMainWithHooks
        (simpleUserHooks
          { postBuild =
              appImageBuildHook
                [ AppImage
                  { appName = "ld52-exe"
                  , appDesktop = "ld52.desktop"
                  , appIcons = ["./resources/textures/chicken.png"]
                  , appResources = do
                      r <- res
                      pure (r, Just $ drop 2 r)
                  , appDirCustomize = Nothing
                  }
                ]
          }
        )
      else do
        putStrLn "WARNING: You are missing either `appimagetool` or `linuxdeploy`, so no AppImage will be built."
        defaultMain

