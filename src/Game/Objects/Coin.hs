module Game.Objects.Coin where

import Game.Common (onHitBy, playerHitRectObj)
import Types


coin :: V2 WorldPos -> Object
coin pos =
  playerHitRectObj
    (\oi ->
      let ev = onHitBy IsPlayer oi
       in mempty
            { oe_die = () <$ ev
            , oe_play_sound = [CoinSound] <$ ev
            }
    )
    (OriginRect 8 4)
    (V4 255 255 0 255)
    pos



