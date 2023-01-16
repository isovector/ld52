module Game.Objects.DespawnTrigger where

import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Monoid
import           Game.Common


despawnTrigger :: V2 WorldPos -> OriginRect Double -> Set ObjectId  -> Object
despawnTrigger pos ore despawns =
  playerHitRectObj'
    (proc oi -> do
      let ev = onHitBy IsPlayer oi
      hit_change <- onChange -< event Nothing Just ev
      let is_hit = hit_change >>= maybeToEvent

      returnA -< mempty
        { oe_die = void is_hit
        , oe_omnipotence = despawnAll despawns <$ is_hit
        }
    )
    ore
    (V4 0 128 92 128)
    pos


despawnAll :: Set ObjectId -> ObjectMap ObjSF -> ObjectMap ObjSF
despawnAll despawns = #objm_map %~ appEndo (foldMap (Endo . M.delete) despawns)

