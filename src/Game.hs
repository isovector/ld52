module Game where

import SDL
import FRP
import Types
import Data.Bool (bool)


logicalSize :: Num a => V2 a
logicalSize = V2 320 240

drawFilledRect :: Color -> Rectangle Int  -> Renderable
drawFilledRect c rect rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap fromIntegral rect

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

game :: SF FrameInfo Renderable
game = do
  let set_bg :: Color -> SF a Renderable
      set_bg col = arr $ pure $ drawBackgroundColor col

  -- The swont monad lets us sequence drawing commands, to run one after another.
  runSwont
      -- When the do block is finished, this is what we will end on, forever after
      -- Namely, here, flickering pink when you press the space bar
      (const $ arr ( bool (V4 0 0 0 255)
                          (V4 255 0 255 255)
                   . c_space
                   . fi_controls
                   ) >>> arr drawBackgroundColor )
      $ do

    -- Change the background to red for a second
    timed 1 $ set_bg $ V4 255 0 0 255

    -- and then to green for a second
    timed 1 $ set_bg $ V4 0 255 0 255

    -- Render this thing until the space bar is pressed:
    waitForEdge (c_space . fi_controls) $
      -- SFs form a monoid, so we can compose renderable things:
      mconcat
        [
          -- Flash ever quarter of a second
          localTime
            >>> arr ((== 0) . (`mod` 2) . round @_ @Int . (* 4))
            >>> arr (bool (V4 0 0 0 0) (V4 255 255 255 50))
            >>> arr drawBackgroundColor

          -- Spin a red rectangle around the screen
        , localTime >>> arr (\t ->
            drawFilledRect (V4 255 0 0 255) $
              Rectangle
                (P $ fmap round
                   $ V2 (cos t) (sin t) * 100
                   + logicalSize / 2
                ) 10
          )
        ]

    -- and then fade to white over 3 seconds.
    lerpSF 3 $ arr $
      -- `lerpSF` causes this `d` to go from 0 to 1 over the 3 second duration
      \d -> drawBackgroundColor $ V4 255 255 255 $ round $ 255 * d

