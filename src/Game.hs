module Game where

import Control.Monad (void)
import Data.Bool (bool)
import FRP
import Game.World (drawWorld)
import SDL
import SDL.Mixer
import Types
import Drawing


logicalSize :: Num a => V2 a
logicalSize = V2 320 240

playSound :: Sound -> Resources -> IO ()
playSound s r = do
  putStrLn "hi"
  halt 0
  void $ playOn 0 Once $ r_sounds r s

data Player = Player
  { p_pos :: V2 Double
  , p_vel :: V2 Double
  }

game :: Resources -> SF FrameInfo Renderable
game rs = -- arr $ const $ drawWorld $ r_worlds rs TestWorld
  do
  loopPre (Player (V2 150 150) (V2 100 100)) $ proc (fi, p) -> do
    let dpos =  fi_dt fi SDL.*^ p_vel p * V2 1 0 * (realToFrac <$> c_dir (fi_controls fi))
    let pos' = p_pos p + dpos
    returnA -< (drawFilledRect (V4 255 0 0 255) $ round <$> Rectangle (P pos') 50, Player pos' (p_vel p))

game' :: SF FrameInfo Renderable
game' = do
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

    -- and play sounds!
    momentary $ playSound NintendoSound

    -- A little interactive section for 5 seconds.
    let run_around :: Resumable Player FrameInfo Renderable
        run_around = Resumable $ proc (p, fi) -> do
          stop <- after 5 () -< ()
          let dpos = pure $ fi_dt fi * bool 0 20 (c_space $ fi_controls fi)
              pos' = p_pos p + dpos
          returnA -< Resumption
            { r_state = Player pos' (p_vel p)
            , r_output = drawFilledRect (V4 255 0 0 255) $ fmap round $ Rectangle (P pos') 10
            , r_stop = stop
            }

    -- We get the resulting world state
    w' <- runResumable (Player 0 0) run_around

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

