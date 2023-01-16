module Game.Objects.Chicken where

import Game.Common

chicken :: V2 WorldPos -> Object
chicken pos0
  = onHitByTag IsPlayer (mconcat
      [ standardDeathResponse
      , addInventoryResponse PowerupDoubleJump
      , playSoundReponse NintendoSound
      ])
  $ staticCollisionObject pos0 ore mempty
  $ \pos -> drawGameTextureOriginRect ChickenTexture ore pos 0
  $ pure False
  where
    ore = mkCenterdOriginRect 40

