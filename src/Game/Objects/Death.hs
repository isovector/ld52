module Game.Objects.Death where

import Game.Common (onHitBy, playerHitRectObj')
import Types
import FRP


deathZone :: V2 WorldPos -> V2 Double -> Object
deathZone pos sz =
  playerHitRectObj'
    (arr $ \oi ->
      let ev = (fmap (, Die) . onHitBy IsPlayer) oi
       in mempty
            { oe_play_sound = [DieSound] <$ ev
            , oe_send_message = fmap pure ev
            }
    )
    (OriginRect (coerce sz) 0)
    (V4 64 0 0 64)
    pos


