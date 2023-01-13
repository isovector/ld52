module Game.Objects.ParticleSpawner where

import Types
import Game.Objects.Particle
import FRP
import Utils (nowish, noObjectState)

particleSpawner :: V2 WorldPos -> ParticleType -> Object
particleSpawner pos pt = proc _ -> do
  start <- nowish () -< ()
  returnA -< ObjectOutput
    { oo_events = mempty
        { oe_die = start
        , oe_spawn = particleType pt pos <$ start
        }
    , oo_render = mempty
    , oo_state = noObjectState pos
    }


particleType :: ParticleType -> V2 WorldPos -> [Object]
particleType Gore = gore
particleType Firework = firework

