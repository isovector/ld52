module Game.Objects.TutorialRegion where

import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import           Engine.Geometry (orTopDist)
import           Game.Common


tutorialRegion :: V2 WorldPos -> OriginRect Double -> Text -> Object
tutorialRegion pos ore (T.unpack -> str) =
  proc oi -> do
    let hits = fmap topOfOre
             $ filter (S.member IsPlayer . os_tags)
             $ fmap snd
             $ event mempty id
             $ oie_hit
             $ oi_events oi

    t <- localTime -< ()
    let onoff = mod @Int (round (t * 2)) 2 == 1
    let on_ore = OriginRect 16 (V2 8 10)
        off_ore = OriginRect 14 (V2 7 9)
        ore' = bool off_ore on_ore onoff

    returnA -< ObjectOutput
      { oo_events = mempty
      , oo_render = flip foldMap hits $ \p ->
          let p' = p + bool (V2 0 2) 0 onoff in
          mconcat
            [ drawGameTextureOriginRect KeycapTexture ore' p' 0 (pure False)
            , drawText (bool 4 6 onoff) (V3 0 0 0) str $ p' - bool (V2 2 6) (V2 3 7) onoff
            ]
      , oo_state = (noObjectState pos)
          { os_collision = Just ore
          }
      }

topOfOre :: ObjectState -> V2 WorldPos
topOfOre os = os_pos os - coerce (orTopDist (fromJust $ os_collision os)) - V2 0 20
