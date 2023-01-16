-- | Here be unutterable, atrocious, ungodly dragons
module Engine.Globals where

import           Data.IORef
import           Engine.Types
import           Game.Resources (loadResources)
import           SDL (Texture)
import qualified Sound.ALUT as ALUT
import           System.IO.Unsafe

veryUnsafeEngineIORef :: IORef Engine
veryUnsafeEngineIORef = unsafePerformIO $ newIORef $ error "no unsafe engine io ref"
{-# NOINLINE veryUnsafeEngineIORef #-}

global_resources :: Resources
global_resources = unsafePerformIO $ loadResources =<< readIORef veryUnsafeEngineIORef
{-# NOINLINE global_resources #-}

global_textures :: GameTexture -> WrappedTexture
global_sounds :: Sound -> ALUT.Source
global_songs :: Song -> ALUT.Source
global_worlds :: WorldName -> World
global_anims :: Anim -> [WrappedTexture]
global_glyphs :: Char -> Texture

Resources
  { r_textures = global_textures
  , r_sounds   = global_sounds
  , r_worlds   = global_worlds
  , r_anims    = global_anims
  , r_glyphs   = global_glyphs
  , r_songs    = global_songs
  } = global_resources


{-# NOINLINE global_anims    #-}
{-# NOINLINE global_glyphs   #-}
{-# NOINLINE global_songs    #-}
{-# NOINLINE global_sounds   #-}
{-# NOINLINE global_textures #-}
{-# NOINLINE global_worlds   #-}

