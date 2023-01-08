{-# OPTIONS_GHC -Wno-orphans #-}

module Resources where

import           Control.Monad ((<=<))
import           Level (loadWorld)
import           Resources.Machinery
import           SDL (Texture, textureWidth, textureHeight)
import qualified SDL.Image as Image
import           SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mixer
import           SDL.Video (queryTexture)
import           Types
import System.FilePath ((</>), (<.>))
import Utils (setGroundOrigin)
import Data.Traversable (for)


frameCounts :: Sprite -> Anim -> Int
frameCounts _ Idle   = 4
frameCounts _ NoAnim = 1
frameCounts _ Run    = 4


wrapTexture :: Texture -> IO WrappedTexture
wrapTexture t = do
  q <- queryTexture t
  pure $ WrappedTexture
    { getTexture = t
    , wt_size = V2 (textureWidth q) $ textureHeight q
    , wt_sourceRect = Nothing
    , wt_origin = 0
    }

instance IsResource (Sprite, Anim) [WrappedTexture] where
  resourceFolder = "sprites"
  resourceExt = "png"
  resourceName _ = "unused"
  load (cn, an) e _ = do
    for [0 .. frameCounts cn an - 1] $ \i -> do
      let fp = "resources" </> "sprites" </> charName cn </> animName an <> "_" <> show i <.> "png"
      wt <- wrapTexture =<< Image.loadTexture (e_renderer e) fp
      pure $ setGroundOrigin wt


animName :: Anim -> FilePath
animName Idle = "idle"
animName NoAnim = "no_anim"
animName Run = "run"


charName :: Sprite -> FilePath
charName MainCharacter = "mc"


instance IsResource Tileset WrappedTexture where
  load _
      = (wrapTexture <=<)
      . Image.loadTexture
      . e_renderer
  resourceFolder = "tilesets"
  resourceExt    = "png"
  resourceName = show

instance IsResource GameTexture WrappedTexture where
  load _
      = (wrapTexture <=<)
      . Image.loadTexture
      . e_renderer
  resourceFolder = "textures"
  resourceExt    = "png"
  resourceName NintendoLogo = "nintendo"

instance IsResource Sound Chunk where
  load _ _ = Mixer.load
  resourceFolder = "sounds"
  resourceExt    = "wav"
  resourceName NintendoSound = "ding"

instance IsResource WorldName World where
  load _ _ = loadWorld
  resourceFolder = "levels"
  resourceExt    = "ldtk"
  resourceName TestWorld = "test"


loadResources :: Engine -> IO Resources
loadResources engine = do
  tilesets <- loadResource engine
  textures <- loadResource engine
  sounds   <- loadResource engine
  worlds   <- loadResource engine
  sprites  <- loadResource engine

  pure $ Resources
    { r_engine   = engine
    , r_tilesets = tilesets
    , r_textures = textures
    , r_sounds   = sounds
    , r_worlds   = worlds
    , r_sprites  = curry sprites
    }

