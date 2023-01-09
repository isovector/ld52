module Game.Objects.Particle where

import Types
import FRP hiding (time)
import Utils
import Drawing (drawOriginRect)
import Collision (move)
import Game.Common (getCollisionMap)
import Data.Maybe (fromMaybe)
import Data.Hashable (hash)

particle
    :: V2 WorldPos
    -> V2 Double
    -> OriginRect Double
    -> Color
    -> V2 Double
    -> Time
    -> Object
particle pos0 vel0 ore col grav life =
    loopPre vel0 $ proc (oi, vel) -> do
      start <- nowish () -< ()
      die <- after life () -< ()

      let pos = event (os_pos $ oi_state oi) (const pos0) start

      let dt = fi_dt $ oi_frameInfo oi

      let gs = fi_global $ oi_frameInfo oi
          mpos' = move (getCollisionMap gs) (coerce ore) pos (vel ^* dt)
          pos' = fromMaybe pos mpos'

          vel' = maybe vel (coerce . subtract pos) (coerce mpos') ^* (1 / dt)
                + grav ^* dt

      let end = mergeEvents
            [ die
            -- , bool noEvent (Event ()) $ quadrance vel' <= 400
            ]

      returnA -< (, vel') $ ObjectOutput
        { oo_events = mempty
            { oe_die = end
            }
        , oo_render = drawOriginRect col (coerce ore) pos'
        , oo_state = (noObjectState pos')
        }


gore :: V2 WorldPos -> [Object]
gore pos = do
  let n = 64
  i <- [id @Int 0 .. n]
  let seed = hash pos + hash i
      j = fromIntegral i * (360 / fromIntegral n)
      speed = 200 + mod (seed * 17) 250
      dur = 8 + mod (seed * 9) 8
      vel = V2 (cos j) (sin j) * fromIntegral speed
  pure
    $ particle pos vel (mkCenterdOriginRect 2) (V4 128 0 0 192) (V2 0 210)
    $ fromIntegral dur

