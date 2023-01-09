module Game.Objects.Door where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types
import Utils (mkGroundOriginRect)


door :: V2 WorldPos -> V2 Double ->  V2 WorldPos -> Object
door pos sz out =
  playerHitRectObjCallback
    (fmap ((, TeleportOpportunity out)) . onHitBy IsPlayer)
    (mkGroundOriginRect $ coerce sz)
    (V4 192 128 0 64)
    pos

