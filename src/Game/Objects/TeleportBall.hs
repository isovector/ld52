module Game.Objects.TeleportBall where

import Types
import FRP
import Utils
import Drawing (drawOriginRect)
import Collision (move)
import Game.Common (getCollisionMap)
import Data.Maybe (fromMaybe)

teleportBall
    :: ObjectId
    -> OriginRect Double
    -> V2 WorldPos
    -> V2 Double
    -> Object
teleportBall owner owner_ore pos0 vel0 = loopPre vel0 $ proc (oi, vel) -> do
  die <- after 1 () -< ()

-- TODO(sandy): this is a bad pattern; object constructor should take an
-- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start
      dt = fi_dt $ oi_frameInfo oi

  let vel' = vel + grav ^* dt
  -- let pos' = pos + coerce (vel ^* dt)
  let ore = mkCenterdOriginRect 4
  let gs = fi_global $ oi_frameInfo oi

      pos' = fromMaybe pos $ move (getCollisionMap gs) (coerce ore) pos (vel ^* dt)

  -- find the last place we could place the character
  youpos'ev <- arr maybeToEvent -<
      move (getCollisionMap gs) (coerce owner_ore) pos (vel ^* dt)
  youpos' <- hold pos0 -< youpos'ev

  returnA -< (, vel') $ ObjectOutput
    { oo_events = mempty
        { oe_die = die
        , oe_send_message = [(owner, TeleportTo youpos')] <$ die
        , oe_focus = () <$ start
        }
    , oo_render = drawOriginRect (V4 0 255 255 255) ore pos'
    , oo_state = (noObjectState pos')
      { os_camera_offset = vel' ^* (dt * 10)
        }
    }


grav :: V2 Double
grav = V2 0 210

