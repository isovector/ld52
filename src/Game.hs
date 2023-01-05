module Game where

import Control.Monad (void)
import Data.Bool (bool)
import FRP
import SDL
import SDL.Mixer
import Types


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

drawTexture :: GameTexture -> Point V2 Int -> V2 Int -> Renderable
drawTexture gt p sz rs = do
  let renderer = e_renderer $ r_engine rs
  copy renderer (r_textures rs gt) Nothing $ Just $ fmap fromIntegral $ Rectangle p sz

playSound :: Sound -> Resources -> IO ()
playSound s r = do
  putStrLn "hi"
  halt 0
  void $ playOn 0 Once $ r_sounds r s

data World = World
  { w_pos :: V2 Double
  }


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

    -- We can draw textures!
    let nintendo =
          mconcat
            [ set_bg $ V4 0 0 0 255
            , arr $ \t ->
                drawTexture NintendoLogo
                  (P $ V2 100 (round $ 120 * t))
                  (V2 120 20)
            ]
    lerpSF 2 nintendo

    -- and play sounds!
    momentary $ playSound NintendoSound
    timed 1 $ constant 1 >>> nintendo

    -- A little interactive section for 5 seconds.
    let run_around :: Resumable World FrameInfo Renderable
        run_around = Resumable $ proc (World pos, fi) -> do
          stop <- after 5 () -< ()
          let dpos = pure $ fi_dt fi * bool 0 20 (c_space $ fi_controls fi)
              pos' = pos + dpos
          returnA -< Resumption
            { r_state = World pos'
            , r_output = drawFilledRect (V4 255 0 0 255) $ fmap round $ Rectangle (P pos') 10
            , r_stop = stop
            }

    -- We get the resulting world state
    w' <- runResumable (World 0) run_around

    -- Change the background to red for a second
    timed 1 $ set_bg $ V4 255 0 0 255

    -- Resume the interactive section where we left off
    void $ runResumable w' run_around

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

