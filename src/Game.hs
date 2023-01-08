{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Control.Lens ((+~))
import           Data.Bool (bool)
import           Data.Foldable (find)
import qualified Data.Map as M
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.World (drawWorld)
import           Globals (global_textures)
import           SDL
import           Types
import           Utils (setCenterOrigin, mkCenterdOriginRect)

#ifndef __HLINT__

initialObjs :: Level -> ObjectMap ObjSF
initialObjs
  = foldr addObject (ObjectMap (ObjectId 0) mempty)
  . l_defaultObjs


game :: Resources -> SF RawFrameInfo (Camera, Renderable)
game rs = loopPre (initialGlobalState rs) $
  proc (RawFrameInfo c dt , gs) -> do
    let fi = FrameInfo c dt gs
    (cam, objs, to_draw) <-
      renderObjects rs (V2 0 0)
        -- BUG(sandy): this should be a signal!!!
        (initialObjs $ gs_currentLevel $ initialGlobalState rs)
          -< fi
    let player = find (S.member IsPlayer . os_tags . oo_state) $ objm_map objs
    bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
    returnA -<
      ( ( cam
        , mconcat
            [ bg
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
      , gs
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
  $ S.singleton Layer1

#endif
