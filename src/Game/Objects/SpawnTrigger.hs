module Game.Objects.SpawnTrigger where

import qualified Data.Map as M
import           Data.Monoid
import           Game.Common


spawnTrigger :: V2 WorldPos -> OriginRect Double -> Bool -> Map Text Object  -> Object
spawnTrigger pos ore persistent spawns =
  playerHitRectObj'
    (proc oi -> do
      let ev = onHitBy IsPlayer oi
      hit_change <- onChange -< event Nothing Just ev
      let is_hit = hit_change >>= maybeToEvent

      returnA -< mempty
        { oe_die = bool (() <$ is_hit) noEvent persistent
        , oe_omnipotence = spawnAll spawns <$ is_hit
        }
    )
    ore
    0
    pos


spawnAll :: Map Text Object -> ObjectMap ObjSF -> ObjectMap ObjSF
spawnAll spawns
  = over #objm_map
  $ appEndo
  $ foldMap (Endo . uncurry M.insert . first StaticId)
  $ M.toList spawns

