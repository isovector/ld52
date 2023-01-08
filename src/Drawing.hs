module Drawing where

import Control.Monad (void)
import FRP
import Foreign.C
import Game.Camera (viaCamera)
import Geometry (orTopLeft)
import Globals (global_resources, global_sprites)
import SDL
import SDL.Mixer
import Types


playSound :: Resources -> Sound -> IO ()
playSound r s = do
  halt 0
  void $ playOn 0 Once $ r_sounds r s

drawOriginRect :: Color -> OriginRect WorldPos -> V2 WorldPos -> Renderable
drawOriginRect c ore pos =
  drawFilledRect c
    $ Rectangle (P $ orTopLeft pos ore)
    $ orect_size ore


drawFilledRect :: Color -> Rectangle WorldPos -> Renderable
drawFilledRect c (Rectangle (P v) sz) cam = do
  let rect' = Rectangle (P $ viaCamera cam v) $ coerce sz
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap (round . getScreenPos) rect'

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c _ = do
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 WorldPos       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched cam = do
  let renderer = e_renderer $ r_engine global_resources
  copyEx
    renderer
    (getTexture wt)
    (wt_sourceRect wt)
    (Just $ fmap round
          $ Rectangle (P $ coerce $ viaCamera cam $ pos - coerce (fmap fromIntegral (wt_origin wt) * stretched))
          $ fmap fromIntegral (wt_size wt) * stretched)
    (CDouble theta)
    (Just $ fmap round
          $ P
          $ fmap fromIntegral (wt_origin wt) * stretched)
    flips

drawSprite
    :: WrappedTexture
    -> V2 WorldPos  -- ^ pos
    -> Double     -- ^ rotation in rads
    -> V2 Bool    -- ^ mirroring
    -> Renderable
drawSprite wt pos theta flips =
  drawSpriteStretched wt pos theta flips 1


mkAnim :: Sprite -> SF (Anim, V2 WorldPos) Renderable
mkAnim sprite = select $ \anim ->
  timedSequence (error "mkAnim: impossible") 0.1
    $ fmap (\wt -> arr $ \pos -> drawSprite wt pos 0 (pure False))
    $ cycle
    $ global_sprites sprite anim

