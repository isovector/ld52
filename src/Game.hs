{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.Objects.Player (mkCenterdOriginRect)
import           Game.World (drawWorld)
import           SDL
import           Types
import Control.Lens ((+~))

#ifndef __HLINT__

initialObjs :: Level -> ObjectMap ObjSF
initialObjs
  = foldr addObject (ObjectMap (ObjectId 0) mempty)
  . l_defaultObjs


game :: Resources -> SF RawFrameInfo (Camera, Renderable)
game rs = loopPre (initialGlobalState rs) $
  proc (RawFrameInfo c dt , gs) -> do
    let fi = FrameInfo c dt gs
    (cam, objs) <-
      renderObjects rs (V2 0 0)
        -- BUG(sandy): this should be a signal!!!
        (initialObjs $ gs_currentLevel $ initialGlobalState rs)
          -< fi
    bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
    returnA -<
      ( ( cam
        , mconcat
            [ bg
            , objs
            , ui_box (-17)
            , ui_box 17
            , drawText 6 (V3 255 0 255) "hello world" 20
            ]
        )
      , gs
      )
  where
    ui_box dx = atScreenPos $
      drawOriginRect (V4 255 255 255 16) (mkCenterdOriginRect 20) $ (logicalSize / 2)
        & _x +~ dx
        & _y .~ 20

initialGlobalState :: Resources -> GlobalState
initialGlobalState rs
  = GlobalState (w_levels (r_worlds rs TestWorld) M.! "AutoLayer")
  $ S.singleton Layer1

#endif
