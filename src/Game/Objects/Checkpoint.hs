module Game.Objects.Checkpoint where

import Game.Common (onHitBy, playerHitRectObj)
import Types

checkpoint :: V2 WorldPos -> Object
checkpoint pos =
  playerHitRectObj
    (\oi ->
      let ev = (fmap (, SetCheckpoint pos) . onHitBy IsPlayer) oi
       in mempty
            { oe_die = () <$ ev
            , oe_play_sound = [CheckpointSound] <$ ev
            , oe_send_message = fmap pure ev
            }
    )
    (OriginRect 8 0)
    (V4 0 255 255 64)
    pos
