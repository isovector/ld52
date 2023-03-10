module Engine.Camera
  ( camera
  , viaCamera
  , getCameraFocus
  ) where

import Engine.Types
import FRP.Yampa


getCameraFocus :: ObjectState -> V2 WorldPos
getCameraFocus os = os_pos os + coerce (os_camera_offset os)

camera
    :: V2 WorldPos
    -> SF (FrameInfo' a, V2 WorldPos)
          Camera
camera = flip loopPre $ arr $ \((fi, focus), pos) -> do
  let dt = fi_dt fi
      pos' = pos + (focus - pos) ^* coerce dt * cameraSpeed
  case focus == pos || distance focus pos <= cameraDeadzone of
    True -> (Camera $ centerScreen pos, pos)
    False -> (Camera $ centerScreen pos', pos')


cameraSpeed :: Num a => a
cameraSpeed = 3


centerScreen :: V2 WorldPos -> V2 WorldPos
centerScreen focus = -focus + logicalSize / 2


viaCamera :: Camera -> V2 WorldPos -> V2 ScreenPos
viaCamera (Camera cam) world = coerce $ cam + world


cameraDeadzone :: Num a => a
cameraDeadzone = 5

