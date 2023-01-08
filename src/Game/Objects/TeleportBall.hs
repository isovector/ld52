module Game.Objects.TeleportBall where

import Types
import FRP
import Utils
import Drawing (drawOriginRect)

teleportBall :: ObjectId -> V2 WorldPos -> V2 Double -> Object
teleportBall owner pos0 vel0 = proc oi -> do
  die <- after 3 () -< ()

-- TODO(sandy): this is a bad pattern; object constructor should take an
-- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start
      dt = fi_dt $ oi_frameInfo oi

  let pos' = traceShowId pos -- + coerce (vel0 ^* dt)

  returnA -< ObjectOutput
    { oo_events = mempty
        { oe_die = die
        , oe_send_message = [(owner, #os_pos .~ pos)] <$ die
        }
    , oo_render = drawOriginRect (V4 0 255 255 255) (mkCenterdOriginRect 4) pos'
    , oo_state = noObjectState pos'
    }
