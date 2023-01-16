module Game.Objects.CollectPowerup where

import Game.Common


collectPowerup :: V2 WorldPos -> PowerupType -> Object
collectPowerup pos pt
  = onHitByTag IsPlayer
      ( mconcat
          [ standardDeathResponse
          , playSoundReponse CoinSound
          , addInventoryResponse pt
          ]
      )
  $ staticCollisionObject pos ore mempty
  $ drawPowerup pt ore pos
  where
    ore = mkCenterdOriginRect 8


drawPowerup :: PowerupType -> OriginRect Double -> V2 WorldPos -> Renderable
drawPowerup pt ore pos = mconcat
  [ drawGameTextureOriginRect (AuraTexture) (mkCenterdOriginRect $ orect_size ore * 3) pos 0 $ pure False
  , drawGameTextureOriginRect (powerupRenderable pt) ore pos 0 $ pure False
  ]


powerupRenderable :: PowerupType -> GameTexture
powerupRenderable PowerupDoubleJump = ChickenTexture
powerupRenderable PowerupWarpBall = TeleTexture
powerupRenderable PowerupTotsugeki = ChickenTexture

