module Drawing where

import Types
import SDL
import Foreign.C

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

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 Double       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched rs = do
  let renderer = e_renderer $ r_engine rs
  copyEx
    renderer
    (getTexture wt)
    (wt_sourceRect wt)
    (Just $ fmap round
          $ Rectangle (P $ pos - (fmap fromIntegral (wt_origin wt) * stretched))
          $ fmap fromIntegral (wt_size wt) * stretched)
    (CDouble theta)
    (Just $ fmap round
          $ P
          $ fmap fromIntegral (wt_origin wt) * stretched)
    flips

drawSprite
    :: WrappedTexture
    -> V2 Double  -- ^ pos
    -> Double     -- ^ rotation in rads
    -> V2 Bool    -- ^ mirroring
    -> Renderable
drawSprite wt pos theta flips =
  drawSpriteStretched wt pos theta flips 1

