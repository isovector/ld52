module Game.Objects.Arrow where

import Game.Common

arrow :: V2 WorldPos -> Double -> Object
arrow pos deg
  = staticCollisionObject pos ore mempty
  $ \p -> drawGameTextureOriginRect ArrowTexture (coerce ore) p deg (pure False)
  where
    ore = mkCenterdOriginRect 24

