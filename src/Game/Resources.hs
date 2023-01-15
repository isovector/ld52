{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Resources where

import           Control.Monad ((<=<))
import           Data.Traversable (for)
import           Engine.Resources
import           Engine.Types
import           Engine.Utils (setGroundOrigin)
import           SDL (Texture, textureWidth, textureHeight)
import           SDL.JuicyPixels (loadJuicyTexture)
import           SDL.Video (queryTexture)
import qualified Sound.ALUT as ALUT
import           System.FilePath ((</>), (<.>))

import {-# SOURCE #-} Engine.Importer (loadWorld)

newtype Char' = Char' { getChar' :: Char }
  deriving (Eq, Ord, Show, Enum)

instance Bounded Char' where
  minBound = Char' $ toEnum 32
  maxBound = Char' $ toEnum 122


frameCounts :: Sprite -> Anim -> Int
frameCounts _ Idle   = 4
frameCounts _ NoAnim = 1
frameCounts _ Run    = 4

frameSound :: Sprite -> Anim -> Int -> Maybe Sound
frameSound _ Run 1 = Just StepSound
frameSound _ Run 3 = Just StepSound
frameSound _ _ _ = Nothing

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
      wt <- wrapTexture =<< loadJuicyTexture (e_renderer e) fp
      pure $ setGroundOrigin wt


animName :: Anim -> FilePath
animName Idle = "idle"
animName NoAnim = "no_anim"
animName Run = "run"


charName :: Sprite -> FilePath
charName MainCharacter = "mc"


loadWrappedTexture :: Engine -> FilePath -> IO WrappedTexture
loadWrappedTexture
  = (wrapTexture <=<)
  . loadJuicyTexture
  . e_renderer

instance IsResource Char' Texture where
  load _
      = loadJuicyTexture
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
      . loadJuicyTexture
      . e_renderer
  resourceFolder = "textures"
  resourceExt    = "png"
  resourceName NintendoLogo = "nintendo"
  resourceName ChickenTexture = "chicken"
  resourceName Parallax0 = "parallax0"
  resourceName Parallax1 = "parallax1"
  resourceName Parallax2 = "parallax2"
  resourceName ChargeTexture = "charge"
  resourceName TeleTexture = "teleball"
  resourceName AuraTexture = "aura"
  resourceName TrampolineTexture = "trampoline"

instance IsResource Song ALUT.Source where
  load _ _ fileName = do
    buf <- ALUT.createBuffer (ALUT.File fileName)
    src <- ALUT.genObjectName
    ALUT.loopingMode src ALUT.$= ALUT.Looping
    ALUT.buffer src ALUT.$= Just buf
    pure src
  resourceFolder = "songs"
  resourceExt    = "wav"
  resourceName WarmDuckShuffle = "warm-duck-shuffle"

instance IsResource Sound ALUT.Source where
  load _ _ fileName = do
    buf <- ALUT.createBuffer (ALUT.File fileName)
    src <- ALUT.genObjectName
    ALUT.loopingMode src ALUT.$= ALUT.OneShot
    ALUT.buffer src ALUT.$= Just buf
    pure src
  resourceFolder = "sounds"
  resourceExt    = "wav"
  resourceName NintendoSound = "ding"
  resourceName CheckpointSound = "checkpoint"
  resourceName CoinSound = "coin"
  resourceName DieSound = "die"
  resourceName JumpSound = "jump"
  resourceName StepSound = "step"
  resourceName ThudSound = "thud"
  resourceName WarpSound = "warp"

instance IsResource WorldName World where
  load _ = loadWorld
  resourceFolder = "levels"
  resourceExt    = "ldtk"
  resourceName TestWorld = "test"
  resourceName HelpWorld = "help"


loadResources :: Engine -> IO Resources
loadResources engine = do
  rpath <- resourceRootPath

  textures <- loadResource rpath engine
  songs    <- loadResource rpath engine
  sounds   <- loadResource rpath engine
  worlds   <- loadResource rpath engine
  sprites  <- loadResource rpath engine
  glyphs   <- loadResource rpath engine

  pure $ Resources
    { r_engine   = engine
    , r_textures = textures
    , r_sounds   = sounds
    , r_songs    = songs
    , r_worlds   = worlds
    , r_sprites  = curry sprites
    , r_glyphs   = glyphs . Char'
    }

