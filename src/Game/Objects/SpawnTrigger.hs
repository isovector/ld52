module Game.Objects.SpawnTrigger where

import Game.Common (onHitBy, playerHitRectObj')
import Types
import FRP


spawnTrigger :: V2 WorldPos -> V2 Double -> Bool -> [Object]  -> Object
spawnTrigger pos sz persistent spawns =
  playerHitRectObj'
    (proc oi -> do
      let ev = onHitBy IsPlayer oi
      hit_change <- onChange -< event Nothing Just ev
      let is_hit = hit_change >>= maybeToEvent

      returnA -< mempty
        { oe_die = bool (() <$ is_hit) noEvent persistent
        , oe_spawn = spawns <$ is_hit
        }
    )
    (OriginRect sz 0)
    (V4 255 0 0 128)
    pos

