module Game.Objects.ToggleRegion where

import           Data.Set (Set)
import qualified Data.Set as S
import           Game.Common


toggleRegion :: V2 WorldPos -> V2 Double -> LevelLayer -> Bool -> Object
toggleRegion pos sz lls toggle =
  proc oi -> do
    let curlls = gs_layerset $ globalState oi
        hit = onHitBy IsPlayer oi
        enable = toggle /= S.member lls curlls

        collision :: Maybe (OriginRect Double)
        collision = bool Nothing (Just $ OriginRect sz 0) enable

    returnA -< ObjectOutput
      { oo_events = mempty
          { oe_omnipotence = (#objm_globalState . #gs_layerset %~ updateLayers toggle lls) <$ hit
          }
      , oo_render = flip (maybe mempty) collision $ \ore ->
          drawOriginRect (V4 255 255 0 32) ore pos
      , oo_state = noObjectState pos & #os_collision .~ collision
      }


updateLayers :: Bool -> LevelLayer -> Set LevelLayer -> Set LevelLayer
updateLayers False = S.delete
updateLayers True = S.insert

