module Game.Objects.Chicken where

import qualified Data.Set as S
import           Drawing (drawSprite)
import           FRP
import           Globals (global_textures)
import           Types
import           Utils (mkCenterdOriginRect, noObjectState)

chicken :: V2 WorldPos -> Object
chicken pos =
  proc oi -> do
    let hit = oie_hit $ oi_events oi
    returnA -< ObjectOutput
      { oo_events =
          mempty
            { oe_die = () <$ hit
            , oe_play_sound = [NintendoSound] <$ hit
            , oe_omnipotence = (#objm_globalState . #gs_layerset %~ S.insert Layer2) <$ hit
            }
      , oo_render =
          drawSprite (global_textures ChickenTexture) pos 0 (pure False)
      , oo_state =
          (noObjectState pos)
            { os_tags = S.singleton $ IsPowerup PowerupDoubleJump
            , os_collision = Just $ mkCenterdOriginRect 40
            }
      }


