module Game.Splash where

import Drawing
import FRP
import Game (game)
import Game.Common


runIntro :: SF RawFrameInfo (Camera, Renderable)
runIntro = runSwont (error "die") $ do
  -- momentary $ (Camera 0, const $ playSong WarmDuckShuffle)
  splashScreen

splashScreen :: Swont RawFrameInfo (Camera, Renderable) ()
splashScreen = do
  swont (liftIntoGame mainMenu) >>= \case
     Start -> swont $ game TestWorld >>> arr (, noEvent)
     Credits -> do
      swont $ liftIntoGame credits
      splashScreen

liftIntoGame :: SF RawFrameInfo (IO (), Event a) -> SF RawFrameInfo ((Camera, Renderable), Event a)
liftIntoGame sf = sf >>> arr (first $ (Camera 0,) . const)



data MenuItem = Start | Credits
  deriving (Eq, Ord, Show, Enum, Bounded)

prevMenuItem :: MenuItem -> MenuItem
prevMenuItem Start = Credits
prevMenuItem n = pred n

nextMenuItem :: MenuItem -> MenuItem
nextMenuItem Credits = Start
nextMenuItem n = succ n


mainMenu :: SF RawFrameInfo (IO (), Event MenuItem)
mainMenu = loopPre Start $ proc (fi, sel) -> do
  ((>>= maybeToEvent) -> y)
      <- onChange -< int2Maybe $ (c_dir $ rfi_controls fi) ^. _y

  let f = bool prevMenuItem nextMenuItem <$> y
  let sel' = event sel ($ sel) f

  press <- edge -< c_z $ rfi_controls fi
  idata <- afterEach (snd $ deltaEncode 0.3 $ cycle
                      [ defaultControls
                          { c_dir = V2 1 0 }
                      , defaultControls
                          { c_dir = V2 1 0 }
                      , defaultControls
                          { c_dir = V2 (-1) 0 }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                          { c_space = True }
                      , defaultControls
                      , defaultControls
                          { c_space = True }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                      , defaultControls
                      , defaultControls
                          { c_dir = V2 (-1) 0
                          }
                      , defaultControls
                          { c_z = True
                          }
                      , defaultControls
                          { c_z = True
                          }
                      , defaultControls
                          { c_z = True
                          }
                      , defaultControls
                      , defaultControls
                      , defaultControls
                      , defaultControls
                          { c_reset = True
                          }
                      ]) -< ()
  inputs <- hold defaultControls -< maybeToEvent =<< idata


  (cam, bggame) <- game HelpWorld -< fi & #rfi_controls .~ inputs

  returnA -<
    ( ( mconcat
          [ bggame cam
          , drawText 16 (V3 0 192 255) "Where's my Chicken," (V2 10 20) (Camera 0)
          , drawText 16 (V3 0 192 255) "man?" (V2 210 38) (Camera 0)
          , drawText 8 (V3 0 128 192) "Hunt of Bounty" (V2 110 70) (Camera 0)
          , mconcat
            $ zipWith (drawMenuItem sel) [minBound .. maxBound] [0..]
          ]
      , sel' <$ press
      )
    , sel'
    )

drawMenuItem :: MenuItem -> MenuItem -> Int -> IO ()
drawMenuItem sel mi ix =
    drawText 16 col (show mi)
      ((logicalSize / 2)
          & _x -~ 130
          & _y .~ 170 + fromIntegral ix * 24
      ) (Camera 0)
  where
    col =
      if sel == mi
         then V3 255 255 255
         else V3 120 120 120


credits :: SF RawFrameInfo (IO (), Event ())
credits = proc rfi -> do
  press <- edge -< c_z $ rfi_controls rfi

  returnA -<
    ( mconcat
        [ drawBackgroundColor (V4 0 0 0 255) (Camera 0)
        , drawText 14 (V3 255 255 255) "Andrew McKnight" (V2 30 70) (Camera 0)
        , drawText 10 (V3 255 255 255) "github.com/amcknight" (V2 30 90) (Camera 0)
        , drawText 14 (V3 255 255 255) "Sandy Maguire" (V2 80 140) (Camera 0)
        , drawText 10 (V3 255 255 255) "github.com/isovector" (V2 80 160) (Camera 0)
        ]
    , press
    )




int2Maybe :: Int -> Maybe Bool
int2Maybe (-1) = Just False
int2Maybe 0 = Nothing
int2Maybe 1 = Just True
int2Maybe _ = Nothing


