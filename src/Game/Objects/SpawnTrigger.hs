module Game.Objects.SpawnTrigger where

import Game.Common


spawnTrigger :: V2 WorldPos -> OriginRect Double -> Bool -> [Object]  -> Object
spawnTrigger pos ore persistent spawns =
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
    ore
    (V4 255 0 0 128)
    pos

