module Game.Objects.CollectPowerup where

import qualified Data.Set as S
import           Game.Common
import           Globals (global_textures)


collectPowerup :: V2 WorldPos -> PowerupType -> Object
collectPowerup pos pt
  = onHitByTag IsPlayer
      ( mconcat
          [ standardDeathResponse
          , playSoundReponse CoinSound
          ]
      )
  $ staticCollisionObject pos ore (S.singleton $ IsPowerup pt)
  $ drawPowerup pt ore pos
  where
    ore = mkCenterdOriginRect 8


drawPowerup :: PowerupType -> OriginRect Double -> V2 WorldPos -> Renderable
drawPowerup pt ore pos = mconcat
  [ drawSpriteOriginRect (global_textures AuraTexture) (mkCenterdOriginRect $ orect_size ore * 3) pos 0 $ pure False
  , drawSpriteOriginRect (powerupRenderable pt) ore pos 0 $ pure False
  ]


powerupRenderable :: PowerupType -> WrappedTexture
powerupRenderable PowerupDoubleJump = global_textures ChickenTexture
powerupRenderable PowerupWarpBall = global_textures TeleTexture

