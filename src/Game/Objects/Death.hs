module Game.Objects.Death where

import Game.Common


deathZone :: V2 WorldPos -> OriginRect Double -> Object
deathZone pos ore
  = onHit (unlessNull . fmap fst) (respondWith Die)
  $ staticCollisionObject pos ore mempty
  $ drawOriginRect (V4 64 0 0 64) (coerce ore) pos

