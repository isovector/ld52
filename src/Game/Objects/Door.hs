module Game.Objects.Door where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types
import Utils (mkGroundOriginRect)
import Drawing (drawOriginRect)


door :: V2 WorldPos -> V2 Double ->  V2 WorldPos -> Object
door pos (mkGroundOriginRect . coerce -> ore) out =
  playerHitRectObjCallback
    (fmap ((, TeleportOpportunity out)) . onHitBy IsPlayer)
    ore
    (drawOriginRect (V4 192 128 0 64) ore)
    pos

