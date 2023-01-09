module Game.Objects.Checkpoint where

import Game.Common (onHitBy, playerHitRectObj)
import Types


checkpoint :: V2 WorldPos -> Object
checkpoint pos =
  playerHitRectObj
    (fmap (pure . (, SetCheckpoint pos)) . onHitBy IsPlayer)
    (OriginRect 8 0)
    (V4 0 255 255 64)
    pos

