module Types
  ( module Types
  , V2 (..)
  , Generic
  , Word8
  , module Debug.Trace
  , SF
  , Event
  ) where

import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Text (Text)
import Data.Word
import Debug.Trace (trace, traceShowId, traceM)
import FRP (SF, Event)
import Foreign.C (CInt)
import GHC.Generics
import SDL hiding (Event)
import SDL.Mixer (Chunk)


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
  , l_tiles  :: LevelLayer -> Resources -> Renderable
  , l_hitmap :: LevelLayer -> V2 Tile -> Bool
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
  , r_tilesets :: Tileset -> WrappedTexture
  , r_textures :: GameTexture -> WrappedTexture
  , r_sounds   :: Sound -> Chunk
  , r_worlds   :: WorldName -> World
  }


------------------------------------------------------------------------------

type Color = V4 Word8

type Renderable = V2 Double -> ScreenRenderable
type ScreenRenderable = Resources -> IO ()


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

data Tileset
  -- NOTE: It's important that the tileset names line up with their png names,
  -- so levels can import them properly.
  = Cavernas_by_Adam_Saltsman
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data WorldName = TestWorld
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------------------------
-- | Audio used by the game.
data Sound = NintendoSound
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LevelLayer
  = Layer1 | Layer2 | Layer3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data WrappedTexture = WrappedTexture
  { getTexture    :: Texture
  , wt_sourceRect :: Maybe (Rectangle CInt)
  , wt_size       :: V2 CInt
  , wt_origin     :: V2 CInt
  }
  deriving stock Generic

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

tileSize :: Num a =>  V2 a
tileSize = 8


------------------------------------------------------------------------------

newtype ObjectId = ObjectId
  { getObjectId :: Int
  }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Enum, Bounded)

type Object = SF ObjectInput ObjectOutput

data ObjectInput = ObjectInput
  -- TODO(sandy): THIS NEVER GETS CALLED YET
  { oi_hit :: Event ()
  , oi_frameInfo :: FrameInfo
  }

data ObjectEvents = ObjectEvents
  { oe_die :: Event ()
  , oe_spawn :: Event [Object]
  , oe_focus :: Event ()
  }

data ObjectOutput = ObjectOutput
  { oo_events :: ObjectEvents
  , oo_render :: Renderable
  , oo_pos :: V2 Double
  }

data ObjectMap a = ObjectMap
  { om_camera :: ObjectId
  , om_map :: Map ObjectId a
  }
  deriving stock (Functor, Generic)


logicalSize :: Num a => V2 a
logicalSize = V2 320 240

