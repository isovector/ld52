module Game.Objects.Checkpoint where

import Game.Common (onHitBy, playerHitRectObj', listenInbox, playerHitRectObj)
import Types
import Control.Lens (preview)
import FRP
import Drawing (drawOriginRect)

checkpoint :: V2 WorldPos -> Object
checkpoint pos =
  playerHitRectObj
    (proc oi -> do
      let hit_cp = (fmap (, SetCheckpoint pos) . onHitBy IsPlayer) oi
          mine = listenInbox (preview #_CurrentCheckpoint . snd) $ oi_events oi

      is_me <- hold False -< fmap (== oi_self oi) mine


      returnA -< (, is_me)
        mempty
          { oe_play_sound = [CheckpointSound] <$ hit_cp
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
