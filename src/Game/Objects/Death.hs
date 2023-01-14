module Game.Objects.Death where

import Game.Common


deathZone :: V2 WorldPos -> OriginRect Double -> Object
deathZone pos ore
  = onHit (unlessNull . fmap fst) (respondWith Die)
  $ staticCollisionObject pos ore mempty
  $ drawOriginRect (V4 64 0 0 64) (coerce ore) pos

--   playerHitRectObj'
--     (proc oi -> do
--       let !ev = onHitBy IsPlayer oi
--       !hit_change <- onChange -< event Nothing Just ev
--       let !is_hit = hit_change >>= maybeToEvent

--       returnA -< mempty
--         { oe_send_message = fmap (pure . (, Die)) is_hit
--         }
--     )
--     ore
--     (V4 255 0 0 128)
--     pos



