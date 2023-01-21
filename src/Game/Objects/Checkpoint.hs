module Game.Objects.Checkpoint where

import Game.Common


checkpoint :: V2 WorldPos -> Object
checkpoint pos
  = id -- oscillate (\t -> coerce $ V2 0 (cos (t * 5) * 1))
  $ playerHitRectObj (proc oi -> do
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
    (\is_me pos' ->
        drawGameTextureOriginRect
          (bool CheckpointTexture ActiveCheckpointTexture is_me)
          ore
          pos'
          0
          (pure False)
    )
    pos
  where
    ore = OriginRect (V2 14 10) 0
