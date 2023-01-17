{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           Engine.Globals
import           Engine.ObjectRouter
import           Engine.Prelude
import           Game.Objects.CollectPowerup (powerupRenderable)
import           Game.World (drawLevel)


#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr (uncurry addStaticObject) (ObjectMap (DynamicId 0) mempty gs mempty)
  $ M.toList
  $ l_defaultObjs $ gs_currentLevel gs


game :: GlobalState -> SF RawFrameInfo (Camera, Renderable)
game gs0 =
  proc rfi -> do
    (cam, objs, to_draw) <-
      renderObjects gs0 (tileToPos $ V2 19 50) (initialObjs gs0)
          -< rfi
    let gs = objm_globalState objs
        levelsz = fmap (fromIntegral . getPixel)
                $ r_size
                $ l_bounds
                $ gs_currentLevel gs

    let drawPowerup :: PowerupType -> V2 ScreenPos -> Renderable
        drawPowerup pt (coerce -> pos) = atScreenPos $ mconcat $
          [ drawOriginRect (V4 255 255 255 16) (mkCenterdOriginRect 20) pos
          , bool
              mempty
              ( atScreenPos $
                  drawSpriteStretched
                    (setCenterOrigin $ global_textures $ powerupRenderable pt)
                    pos
                    0
                    (pure False)
                    0.2
              ) $ S.member pt $ gs_inventory $ gameState gs
          ]

    bg <- arr $ uncurry drawLevel -< (gs_layerset gs, gs_currentLevel gs)

    returnA -<
      ( cam
      , mconcat
          [ drawBackgroundColor (V4 46 90 137 255)
          , drawParallax levelsz Parallax0 3
          , drawParallax levelsz Parallax1 4
          , drawParallax levelsz Parallax2 5
          , bg
          , to_draw
          , drawPowerup PowerupWarpBall $ ui_box_pos (-17)
          , drawPowerup PowerupTotsugeki $ ui_box_pos (17)
          , atScreenPos
              $ drawText 8
                  (V3 255 255 0)
                  (show $ gs_coins $ gameState gs)
              $ V2 10 10
          ]
        )
  where
    ui_box_pos dx =
      (logicalSize / 2)
        & _x +~ dx
        & _y .~ 20

initialGlobalState :: WorldName -> GlobalState
initialGlobalState w
  = GlobalState
      (w_levels (global_worlds w) M.! "AutoLayer")
      (S.fromList [Layer3])
      (GameState 0 mempty)

#endif

