module Game.Objects.Checkpoint where

import Game.Common


checkpoint :: V2 WorldPos -> Object
checkpoint pos =
  playerHitRectObj
    (proc oi -> do
      let hit_cp = (fmap (, SetCheckpoint pos) . onHitBy IsPlayer) oi
          mine = listenInbox (preview #_CurrentCheckpoint . snd) $ oi_events oi

      is_me <- dHold False -< fmap (== oi_self oi) mine
      newly_hit <- edge -< is_me

      returnA -< (, is_me)
        mempty
          { oe_play_sound = [CheckpointSound] <$ newly_hit
          , oe_send_message = fmap pure hit_cp
          }
    )
    ore
    (\is_me pos' -> drawOriginRect (bool off_color on_color is_me) ore pos')
    pos
  where
    ore = OriginRect 8 0
    on_color = V4 255 0 255 255
    off_color = V4 100 0 100 255
