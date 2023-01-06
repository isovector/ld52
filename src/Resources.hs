{-# OPTIONS_GHC -Wno-orphans #-}

module Resources where

import           Resources.Machinery
import           SDL (Texture)
import qualified SDL.Image as Image
import           SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mixer
import           Types
import Level (loadWorld)


instance IsResource GameTexture Texture where
  load = Image.loadTexture . e_renderer
  resourceFolder = "textures"
  resourceExt    = "png"
  resourceName NintendoLogo = "nintendo"

instance IsResource Sound Chunk where
  load _ = Mixer.load
  resourceFolder = "sounds"
  resourceExt    = "wav"
  resourceName NintendoSound = "ding"

instance IsResource WorldName World where
  load _ = loadWorld
  resourceFolder = "levels"
  resourceExt    = "ldtk"
  resourceName TestWorld = "test"


loadResources :: Engine -> IO Resources
loadResources engine = do
  textures <- loadResource engine
  sounds   <- loadResource engine
  worlds   <- loadResource engine

  pure $ Resources
    { r_engine   = engine
    , r_textures = textures
    , r_sounds   = sounds
    , r_worlds   = worlds
    }

