module Game.Objects.ParticleSpawner where

import Types
import Game.Objects.Particle
import Game.Common

particleSpawner :: V2 WorldPos -> ParticleType -> Object
particleSpawner pos pt
  = onSpawn (mconcat
      [ standardDeathResponse
      , spawnResponse $ particleType pt pos
      ])
  $ staticObject pos mempty mempty


particleType :: ParticleType -> V2 WorldPos -> [Object]
particleType Gore = gore
particleType Firework = firework

