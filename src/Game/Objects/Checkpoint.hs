module Game.Objects.Checkpoint where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types


checkpoint :: V2 WorldPos -> Object
checkpoint pos =
  playerHitRectObjCallback
    (fmap (, SetCheckpoint pos) . onHitBy IsPlayer)
    (OriginRect 8 0)
    (V4 0 255 255 64)
    pos

