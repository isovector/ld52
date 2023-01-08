module Game.Objects.Chicken where

import Types
import FRP
import Drawing (drawSprite)
import Globals (global_textures)

chicken :: V2 WorldPos -> Object
chicken pos =
  Object
    (ObjectMeta {om_tags = mempty, om_hitSize = Just $ V2 40 44}) $
    proc oi -> do
      let hit = oi_hit oi
      returnA -< ObjectOutput
        { oo_events =
            mempty
              { oe_die = () <$ hit
              , oe_play_sound = [NintendoSound] <$ hit
              }
        , oo_render =
            drawSprite (global_textures ChickenTexture) pos 0 (pure False)
        , oo_pos = pos
        }


