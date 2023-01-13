module Game.Objects.CollectPowerup where

import qualified Data.Set as S
import           Drawing (drawSpriteOriginRect)
import           FRP
import           Game.Common (onHitBy, playerHitRectObj)
import           Globals (global_textures)
import           Types
import           Utils (mkCenterdOriginRect)


collectPowerup :: V2 WorldPos -> PowerupType -> Object
collectPowerup pos pt =
  playerHitRectObj
    (arr $ \oi ->
      let ev = onHitBy IsPlayer oi
       in (, ()) $ mempty
            { oe_die = () <$ ev
            , oe_play_sound = [CoinSound] <$ ev
            }
    )
    ore
    (const $ drawPowerup pt ore)
    pos
  >>> arr (#oo_state . #os_tags <>~ S.singleton (IsPowerup pt))
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

