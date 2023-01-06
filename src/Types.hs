module Types
  ( module Types
  , V2 (..)
  , Generic
  , Word8
  , module Debug.Trace
  ) where

import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import SDL
import SDL.Mixer (Chunk)
import Debug.Trace (trace, traceShowId, traceM)


------------------------------------------------------------------------------

data Rect a = Rect
  { r_pos :: V2 a
  , r_size :: V2 a
  }
  deriving stock (Eq, Ord, Show, Read, Functor)


newtype Tile = Tile
  { getTile :: Int
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num)

newtype Pixel = Pixel
  { getPixel :: Int
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num)

data World = World
  { w_levels :: Map Text Level
  }

data Level = Level
  { l_bgcolor :: Color
  , l_tilebounds :: Rect Tile
  , l_bounds  :: Rect Pixel
  , l_hitmap  :: V2 Tile -> Bool
  }
  deriving stock Generic



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
  , r_worlds   :: WorldName -> World
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


data WorldName = TestWorld
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------------------------
-- | Audio used by the game.
data Sound = NintendoSound
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | Input for the frame.
data Controls = Controls
  { c_space :: Bool
  }

defaultControls :: Controls
defaultControls = Controls
  { c_space = False
  }

