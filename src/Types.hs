module Types
  ( module Types
  , V2 (..)
  , Generic
  , Word8
  ) where

import SDL
import GHC.Generics
import Data.Word
import SDL.Mixer (Chunk)


------------------------------------------------------------------------------

data Engine = Engine
  { e_renderer :: Renderer
  , e_window :: Window
  }

------------------------------------------------------------------------------
-- | Things we need to keep track of, like sprites and music and stuff.
data Resources = Resources
  { r_engine   :: Engine
  , r_textures :: GameTexture -> Texture
  , r_sounds   :: Sound -> Chunk
  }


------------------------------------------------------------------------------

type Color = V4 Word8
type Renderable = Resources -> IO ()


------------------------------------------------------------------------------
-- | Things that change every frame.
data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  }
  deriving stock Generic


------------------------------------------------------------------------------
-- | Textures used by the game.
data GameTexture = NintendoLogo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | Audio used by the game.
data Sound = NintendoSound
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | Input for the frame.
data Controls = Controls
  { c_space :: Bool
  , c_dir :: V2 Int
  }

defaultControls :: Controls
defaultControls = Controls
  { c_space = False
  , c_dir = V2 0 0
  }

