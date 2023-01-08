{-# OPTIONS_GHC -Wno-orphans #-}

module Resources where

import           Control.Monad ((<=<))
import           Data.Traversable (for)
import           Resources.Machinery
import           SDL (Texture, textureWidth, textureHeight)
import qualified SDL.Image as Image
import           SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mixer
import           SDL.Video (queryTexture)
import           System.FilePath ((</>), (<.>))
import           Types
import           Utils (setGroundOrigin)

import {-# SOURCE #-} Level (loadWorld)

newtype Char' = Char' { getChar' :: Char }
  deriving (Eq, Ord, Show, Enum)

instance Bounded Char' where
  minBound = Char' $ toEnum 32
  maxBound = Char' $ toEnum 122


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

instance IsResource Char' Texture where
  load _
      = Image.loadTexture
      . e_renderer
  resourceFolder = "font"
  resourceExt    = "png"
  resourceName c = "font-" <> pad 3 '0' (show $ fromEnum c)

pad :: Int -> Char -> String -> String
pad n c s =
  let len = length s
   in case len >= n of
        True -> s
        False -> replicate (n - len) c <> s

instance IsResource GameTexture WrappedTexture where
  load _
      = (wrapTexture <=<)
      . Image.loadTexture
      . e_renderer
  resourceFolder = "textures"
  resourceExt    = "png"
  resourceName NintendoLogo = "nintendo"
  resourceName ChickenTexture = "chicken"

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
  glyphs   <- loadResource engine

  pure $ Resources
    { r_engine   = engine
    , r_tilesets = tilesets
    , r_textures = textures
    , r_sounds   = sounds
    , r_worlds   = worlds
    , r_sprites  = curry sprites
    , r_glyphs   = glyphs . Char'
    }

