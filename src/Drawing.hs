{-# LANGUAGE CPP               #-}

#ifndef __HLINT__

module Drawing where

import Control.Monad (void)
import Data.Foldable (for_, traverse_)
import FRP
import Foreign.C
import Game.Camera (viaCamera)
import Globals (global_resources, global_sprites, global_glyphs, global_textures)
import SDL
import SDL.Mixer
import Types
import Utils (originRectToRect)
import Geometry (rectContains)
import Resources (frameSound)


playSound :: Resources -> Sound -> IO ()
playSound r s = do
  halt 0
  void $ playOn 0 Once $ r_sounds r s


drawOriginRect :: Color -> OriginRect WorldPos -> V2 WorldPos -> Renderable
drawOriginRect c ore = drawFilledRect c . originRectToRect ore


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
drawSpriteStretched wt pos theta flips stretched cam
  | let wp = viaCamera cam $ pos - coerce (fmap fromIntegral (wt_origin wt) * stretched)
  , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round
              $ Rectangle (P $ coerce wp)
              $ fmap fromIntegral (wt_size wt) * stretched)
        (CDouble theta)
        (Just $ fmap round
              $ P
              $ fmap fromIntegral (wt_origin wt) * stretched)
        flips
  | otherwise = mempty

drawSpriteOriginRect
    :: WrappedTexture  -- ^ Texture
    -> OriginRect WorldPos
    -> V2 WorldPos     -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawSpriteOriginRect wt ore pos theta flips cam
  | let wp = viaCamera cam pos
  , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round $ originRectToRect ore $ coerce wp)
        (CDouble theta)
        (Just $ P $ fmap round wp)
        flips
  | otherwise = mempty

drawSprite
    :: WrappedTexture
    -> V2 WorldPos  -- ^ pos
    -> Double     -- ^ rotation in rads
    -> V2 Bool    -- ^ mirroring
    -> Renderable
drawSprite wt pos theta flips =
  drawSpriteStretched wt pos theta flips 1


mkAnim :: Sprite -> SF (DrawSpriteDetails, V2 WorldPos) Renderable
mkAnim sprite = select $ \dsd ->
  timedSequence (error "mkAnim: impossible") 0.1
    $ fmap (\(i, wt) ->
      proc pos -> do
        i_changed <- edgeBy (\oi ni -> bool Nothing (Just ()) (oi /= ni)) 999 -< i
        returnA -< \cam -> do
          !_ <- traverse_ (playSound global_resources) $ do
            !s <- frameSound sprite (dsd_anim dsd) i <$ i_changed
            maybeToEvent s
          drawSprite wt pos (dsd_rotation dsd) (dsd_flips dsd) cam
          )
    $ cycle
    $ zip [0..]
    $ global_sprites sprite
    $ dsd_anim dsd


atScreenPos :: Renderable -> Renderable
atScreenPos f _ = f $ Camera 0


drawText :: Double -> V3 Word8 -> String -> V2 WorldPos -> Renderable
drawText sz color text pos@(V2 x y) cam
  | rectContains screenRect $ viaCamera cam pos
  = do
      let renderer = e_renderer $ r_engine global_resources
      for_ (zip text [0..]) $ \(c, i) -> do
        let glyph = global_glyphs c
        textureColorMod glyph $= color
        copy renderer glyph Nothing
          $ Just
          $ fmap round
          $ Rectangle (P $ coerce $ viaCamera cam $ V2 (x + coerce (i * sz)) y)
          $ V2 sz sz
      rendererDrawBlendMode renderer $= BlendAlphaBlend
  | otherwise = mempty

drawParallax :: V2 WorldPos -> GameTexture -> Double -> Renderable
drawParallax sz gt scale c@(Camera cam) =
  flip atScreenPos c
    $ drawSpriteOriginRect (global_textures gt) (coerce bg_ore) (logicalSize / 2) 0
    $ pure False
  where
    perc = coerce $ -cam / sz
    bg_ore = OriginRect (logicalSize ^* scale) ((logicalSize ^* scale) * perc)

#endif
