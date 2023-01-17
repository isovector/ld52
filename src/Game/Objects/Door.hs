module Game.Objects.Door where

import Game.Common


door :: V2 WorldPos -> OriginRect Double ->  V2 WorldPos -> Object
door pos ore out
  = onHit (Just . fmap fst) (respondWith (TeleportOpportunity out))
  $ staticCollisionObject pos ore mempty
  $ drawOriginRect (V4 34 15 4 255) ore

