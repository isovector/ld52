module Game.Objects.Chicken where

import qualified Data.Set as S
import           Game.Common
import           Globals (global_textures)

chicken :: V2 WorldPos -> Object
chicken pos
  = onHitByTag IsPlayer (mconcat
      [ standardDeathResponse
      , playSoundReponse NintendoSound
      , omnipotenceResponse $ #objm_globalState . #gs_layerset %~ S.insert Layer2
      ])
  $ staticCollisionObject pos ore (S.singleton $ IsPowerup PowerupDoubleJump)
  $ drawSpriteOriginRect (global_textures ChickenTexture) ore pos 0
  $ pure False
  where
    ore = mkCenterdOriginRect 40

