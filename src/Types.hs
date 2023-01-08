{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  ( module Types
  , V2 (..)
  , V3 (..)
  , V4 (..)
  , Rectangle (..)
  , Point (..)
  , Generic
  , Word8
  , module Debug.Trace
  , SF
  , Event
  , coerce
  , module Control.Lens
  , (*^)
  , (^*)
  , _x
  , _y
  , distance
  , toList
  ) where

import Control.Lens ((&), (^.), (.~), (%~), view, set, over)
import Data.Coerce
import Data.Functor.Compose (Compose)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import Debug.Trace (trace, traceShowId, traceM)
import FRP (SF, Event)
import Foreign.C (CInt)
import GHC.Generics
import SDL hiding (Event)
import SDL.Mixer (Chunk)
import Data.Foldable (toList)


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

newtype ScreenPos = ScreenPos
  { getScreenPos :: Double
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Fractional, Floating, Real, RealFrac)

newtype WorldPos = WorldPos
  { getWorldPos :: Double
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Fractional, Floating, Real, RealFrac)

data World = World
  { w_levels :: Map Text Level
  }

data Level = Level
  { l_bgcolor :: Color
  , l_tilebounds :: Rect Tile
  , l_bounds  :: Rect Pixel
  , l_tiles  :: LevelLayer -> Resources -> Renderable
  , l_hitmap :: LevelLayer -> V2 Tile -> Bool
  , l_defaultObjs :: [Object]
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
  , r_sprites  :: Sprite -> Anim -> [WrappedTexture]
  }


------------------------------------------------------------------------------

type Color = V4 Word8

type Renderable = Camera -> Resources -> IO ()


------------------------------------------------------------------------------
-- | Things that change every frame.
data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  , fi_global :: GlobalState
  }
  deriving stock Generic

-- TODO(sandy): stupid duplicate
data RawFrameInfo = RawFrameInfo
  { rfi_controls :: Controls
  , rfi_dt :: Double
  }
  deriving stock Generic


------------------------------------------------------------------------------
-- | Textures used by the game.
data GameTexture
    = NintendoLogo
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

tileSize :: Num a => a
tileSize = 8


------------------------------------------------------------------------------

newtype ObjectId = ObjectId
  { getObjectId :: Int
  }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Enum, Bounded)

type ObjSF = SF ObjectInput ObjectOutput

type Object = WithMeta ObjSF

data ObjectTag = IsPlayer | IsPowerup
  deriving (Eq, Ord, Show, Enum, Bounded)

data ObjectMeta = ObjectMeta
  { om_tags :: Set ObjectTag
  -- TODO(sandy): maybe this should be an objOut?
  , om_hitSize :: Maybe (V2 Double)
  }
  deriving stock (Eq, Ord, Show)

noObjectMeta :: ObjectMeta
noObjectMeta = ObjectMeta mempty (Just 8)

data WithMeta a = Object
  { obj_metadata :: ObjectMeta
  , obj_data :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable)

type HitEvent = (ObjectId, ObjectMeta)

data ObjectInput = ObjectInput
  { oi_hit :: Event [HitEvent]
  , oi_frameInfo :: FrameInfo
  }
  deriving stock Generic

data GlobalState = GlobalState
  { gs_currentLevel :: Level
  , gs_layerset :: Set LevelLayer
  }
  deriving stock Generic

data ObjectEvents = ObjectEvents
  { oe_die :: Event ()
  , oe_spawn :: Event [Object]
  , oe_focus :: Event ()
  , oe_play_sound :: Event [Sound]
  }
  deriving stock Generic

instance Semigroup ObjectEvents where
  (ObjectEvents ev ev' ev2 ev3) <> (ObjectEvents ev4 ev5 ev6 ev7)
    = ObjectEvents
        {oe_die = ev <> ev4, oe_spawn = ev' <> ev5, oe_focus = ev2 <> ev6,
         oe_play_sound = ev3 <> ev7}

instance Monoid ObjectEvents where
  mempty
    = ObjectEvents
        {oe_die = mempty, oe_spawn = mempty, oe_focus = mempty,
         oe_play_sound = mempty}

data ObjectOutput = ObjectOutput
  { oo_events :: ObjectEvents
  , oo_render :: Renderable
  , oo_pos :: V2 WorldPos
  }
  deriving stock Generic

data ObjectMap a = ObjectMap
  { objm_camera_focus :: ObjectId
  , objm_map :: Compose (Map ObjectId) WithMeta a
  }
  deriving stock (Functor, Generic)

data OriginRect aff = OriginRect
  { orect_size   :: V2 aff
  , orect_offset :: V2 aff
  }
  deriving (Eq, Ord, Show, Functor, Generic)

newtype Camera = Camera (V2 WorldPos)

logicalSize :: Num a => V2 a
logicalSize = V2 320 240

-- WHY DOESNT THIS EXIST
instance (Bounded b, Enum a, Enum b) => Enum (a, b) where
  toEnum n =
    let a = n `div` (1 + fromEnum (maxBound @b))
        b = n `mod` (1 + fromEnum (maxBound @b))
     in (toEnum a, toEnum b)
  fromEnum (a, b) = fromEnum a * (1 + fromEnum (maxBound @b)) + fromEnum b


data Sprite
  = MainCharacter
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)


data Anim
  = Idle
  | NoAnim
  | Run
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

