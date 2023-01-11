import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.AppImage

main =
  defaultMainWithHooks
    (simpleUserHooks
      { postBuild =
          appImageBuildHook
            [ AppImage
              { appName = "ld52-exe"
              , appDesktop = "ld52.desktop"
              , appIcons = ["./resources/textures/chicken.png"]
              , appResources = [("./resources/textures/chicken.png", Nothing)]
              , appDirCustomize = Nothing
              }
            ]
      }
    )

