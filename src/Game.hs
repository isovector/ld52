{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Control.Lens ((+~))
import           Data.Foldable (find)
import qualified Data.Map as M
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.World (drawLevel)
import           Globals (global_textures)
import           SDL
import           Types
import           Utils (setCenterOrigin, mkCenterdOriginRect)

#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr addObject (ObjectMap (ObjectId 0) mempty gs mempty)
  $ l_defaultObjs $ gs_currentLevel gs


game :: Resources -> SF RawFrameInfo (Camera, Renderable)
game rs =
  proc rfi -> do
    (cam, objs, to_draw) <-
      renderObjects rs (V2 0 0)
        -- BUG(sandy): this should be a signal!!!
        (initialObjs $ initialGlobalState rs)
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
          [ drawParallax levelsz Parallax0 3
          , drawParallax levelsz Parallax1 4
          , drawParallax levelsz Parallax2 5
          , bg
          , to_draw
          , ui_box (-17)
          , maybe mempty
              ( bool mempty
                  ( atScreenPos $
                      drawSpriteStretched
                        (setCenterOrigin $ global_textures ChickenTexture)
                        (ui_box_pos (-17))
                        0
                        (pure False)
                        0.3
                  )
                . hasChicken
              ) player
          , ui_box 17
          , drawText 6 (V3 255 0 255) "hello world" 20
          ]
        )
  where
    ui_box_pos dx =
      (logicalSize / 2)
        & _x +~ dx
        & _y .~ 20
    ui_box dx = atScreenPos $
      drawOriginRect (V4 255 255 255 16) (mkCenterdOriginRect 20) $ ui_box_pos dx

hasChicken :: ObjectOutput -> Bool
hasChicken
    = S.member (HasPowerup PowerupDoubleJump) . os_tags . oo_state

initialGlobalState :: Resources -> GlobalState
initialGlobalState rs
  = GlobalState (w_levels (r_worlds rs TestWorld) M.! "AutoLayer")
  $ S.fromList [Layer1, Layer3]

#endif
