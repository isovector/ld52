module Game.Objects.Chicken where

import Game.Common

chicken :: V2 WorldPos -> Object
chicken pos
  = onHitByTag IsPlayer (mconcat
      [ standardDeathResponse
      , addInventoryResponse PowerupDoubleJump
      , playSoundReponse NintendoSound
      ])
  $ staticCollisionObject pos ore mempty
  $ drawGameTextureOriginRect ChickenTexture ore pos 0
  $ pure False
  where
    ore = mkCenterdOriginRect 40

