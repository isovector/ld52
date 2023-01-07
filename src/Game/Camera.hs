module Game.Camera
  ( camera
  , viaCamera
  ) where

import Types
import FRP.Yampa
import SDL (distance, (^*))

camera
    :: V2 WorldPos
    -> SF (FrameInfo, V2 WorldPos)
          Camera
camera = flip loopPre $ arr $ \((fi, focus), pos) -> do
  let dt = fi_dt fi
      pos' = pos + (focus - pos) ^* coerce dt * 2
  case focus == pos || distance focus pos <= cameraDeadzone of
    True -> (Camera $ centerScreen pos, pos)
    False -> (Camera $ centerScreen pos', pos')


centerScreen :: V2 WorldPos -> V2 WorldPos
centerScreen focus = -focus + logicalSize / 2


viaCamera :: Camera -> V2 WorldPos -> V2 ScreenPos
viaCamera (Camera cam) world = coerce $ cam + world


cameraDeadzone :: Num a => a
cameraDeadzone = 60

