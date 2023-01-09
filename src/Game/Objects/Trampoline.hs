module Game.Objects.Trampoline where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types


trampoline :: V2 WorldPos -> V2 Double -> Double -> Object
trampoline pos sz str =
  playerHitRectObjCallback
    (fmap (, OnTrampoline str) . onHitBy IsPlayer)
    (OriginRect (coerce sz) 0)
    (V4 0 128 0 64)
    pos

