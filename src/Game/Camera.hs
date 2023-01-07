module Game.Camera
  ( camera
  , viaCamera
  ) where

import Types
import FRP.Yampa
import SDL (distance, (^*))
import Data.Coerce (coerce)

camera
    :: V2 WorldPos
    -> SF (FrameInfo, V2 WorldPos)
          (V2 ScreenPos)
camera = flip loopPre $ arr $ \((fi, focus), pos) -> do
  let dt = fi_dt fi
      pos' = pos + (focus - pos) ^* coerce dt * 2
  case focus == pos || distance focus pos <= cameraDeadzone of
    True -> (centerScreen pos, pos)
    False -> (centerScreen pos', pos')


centerScreen :: V2 WorldPos -> V2 ScreenPos
centerScreen (coerce -> focus) = -focus + logicalSize / 2


viaCamera :: V2 ScreenPos -> V2 WorldPos -> V2 ScreenPos
viaCamera cam world = cam + coerce world


cameraDeadzone :: Num a => a
cameraDeadzone = 60

