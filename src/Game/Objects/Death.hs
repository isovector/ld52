module Game.Objects.Death where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types


deathZone :: V2 WorldPos -> V2 Double -> Object
deathZone pos sz =
  playerHitRectObjCallback
    (fmap (, Die) . onHitBy IsPlayer)
    (OriginRect (coerce sz) 0)
    (V4 64 0 0 64)
    pos


