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

data Player = Player
  { p_pos :: V2 Double
  , p_vel :: V2 Double
  }

game :: SF FrameInfo Renderable
game = do
  loopPre (Player (V2 150 150) (V2 100 100)) $ proc (fi, p) -> do
    let dpos =  fi_dt fi SDL.*^ p_vel p * V2 1 0 * (realToFrac <$> c_dir (fi_controls fi))
    let pos' = p_pos p + dpos
    returnA -< (drawFilledRect (V4 255 0 0 255) $ round <$> Rectangle (P pos') 50, Player pos' (p_vel p))

