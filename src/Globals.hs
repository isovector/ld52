-- | Here be unutterable, atrocious, ungodly dragons
module Globals where

import Data.IORef
import Resources (loadResources)
import System.IO.Unsafe
import Types
import qualified Sound.ALUT as ALUT
import SDL (Texture)

veryUnsafeEngineIORef :: IORef Engine
veryUnsafeEngineIORef = unsafePerformIO $ newIORef $ error "no unsafe engine io ref"
{-# NOINLINE veryUnsafeEngineIORef #-}

global_resources :: Resources
global_resources = unsafePerformIO $ loadResources =<< readIORef veryUnsafeEngineIORef
{-# NOINLINE global_resources #-}

global_tilesets :: Tileset -> WrappedTexture
global_textures :: GameTexture -> WrappedTexture
global_sounds :: Sound -> ALUT.Source
global_songs :: Song -> ALUT.Source
global_worlds :: WorldName -> World
global_sprites :: Sprite -> Anim -> [WrappedTexture]
global_glyphs :: Char -> Texture

Resources
  { r_tilesets = global_tilesets
  , r_textures = global_textures
  , r_sounds   = global_sounds
  , r_worlds   = global_worlds
  , r_sprites  = global_sprites
  , r_glyphs   = global_glyphs
  , r_songs    = global_songs
  } = global_resources


{-# NOINLINE global_tilesets #-}
{-# NOINLINE global_textures #-}
{-# NOINLINE global_sounds   #-}
{-# NOINLINE global_worlds   #-}
{-# NOINLINE global_sprites  #-}
{-# NOINLINE global_glyphs  #-}
{-# NOINLINE global_songs  #-}

