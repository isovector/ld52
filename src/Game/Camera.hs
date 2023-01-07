module Game.Camera where

import Types
import FRP.Yampa
import SDL (distance, (^*))


camera
    :: V2 Double   -- ^ initial
    -> SF (FrameInfo, V2 Double)  -- desired world pos
          (V2 Double)  -- screen offset
camera = flip loopPre $ arr $ \((fi, focus), pos) -> do
  let dt = fi_dt fi
      pos' = pos + (focus - pos) ^* dt * 2
  case focus == pos || distance focus pos <= cameraDeadzone of
    True -> (centerScreen pos, pos)
    False -> (centerScreen pos', pos')


centerScreen :: V2 Double -> V2 Double
centerScreen focus = -focus + logicalSize / 2


cameraDeadzone :: Double
cameraDeadzone = 60

