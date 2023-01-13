module Game.Objects.Door where

import Game.Common


door :: V2 WorldPos -> V2 Double ->  V2 WorldPos -> Object
door pos (mkGroundOriginRect -> ore) out
  = onHit (Just . fmap fst) (respondWith (TeleportOpportunity out))
  $ staticCollisionObject pos ore mempty
  $ drawOriginRect (V4 192 128 0 64) ore pos

