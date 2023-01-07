module Drawing where

import Types
import SDL
import Foreign.C
import Game.Camera (viaCamera)
import Data.Coerce (coerce)


drawFilledRect :: Color -> Rectangle WorldPos -> Renderable
drawFilledRect c (Rectangle (P v) sz) cam rs = do
  let rect' = Rectangle (P $ viaCamera cam v) $ coerce sz
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap (round . getScreenPos) rect'

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c _ rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 WorldPos       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched cam rs = do
  let renderer = e_renderer $ r_engine rs
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

