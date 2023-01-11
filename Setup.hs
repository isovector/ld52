import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.AppImage
import System.Directory.Recursive

main :: IO ()
main = do
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

