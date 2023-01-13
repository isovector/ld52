{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           Engine.Globals
import           Engine.ObjectRouter
import           Engine.Prelude
import           Game.World (drawLevel)


#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr addObject (ObjectMap (ObjectId 0) mempty gs mempty)
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
    let player = find (S.member IsPlayer . os_tags . oo_state) $ objm_map objs

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
          , ui_box (17)
          , maybe mempty
              ( bool mempty
                  ( atScreenPos $
                      drawSpriteStretched
                        (setCenterOrigin $ global_textures ChickenTexture)
                        (ui_box_pos (17))
                        0
                        (pure False)
                        0.3
                  )
                . hasPowerup PowerupDoubleJump
              ) player
          , maybe mempty
              ( bool mempty
                  ( atScreenPos $
                      drawSpriteStretched
                        (setCenterOrigin $ global_textures TeleTexture)
                        (ui_box_pos (-17))
                        0
                        (pure False)
                        0.2
                  )
                . hasPowerup PowerupWarpBall
              ) player
          , ui_box (-17)
          ]
        )
  where
    ui_box_pos dx =
      (logicalSize / 2)
        & _x +~ dx
        & _y .~ 20
    ui_box dx = atScreenPos $
      drawOriginRect (V4 255 255 255 16) (mkCenterdOriginRect 20) $ ui_box_pos dx

hasPowerup :: PowerupType -> ObjectOutput -> Bool
hasPowerup pu
    = S.member (HasPowerup pu) . os_tags . oo_state

initialGlobalState :: WorldName -> GlobalState
initialGlobalState w
  = GlobalState
      (w_levels (global_worlds w) M.! "AutoLayer")
      (S.fromList [Layer3])
      (GameState 0 mempty)

#endif

