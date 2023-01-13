{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , bool
  ) where

import Control.Lens ((&), (^.), (.~), (%~), (+~), (-~), (<>~), view, set, over, preview, review)
import Data.Bool (bool)
import Data.Coerce
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Monoid (Endo(Endo), appEndo)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import Debug.Trace (trace, traceShowId, traceM)
import FRP (SF, Event, Time)
import Foreign.C (CInt)
import GHC.Generics
import SDL hiding (trace, Event)
import SDL.Mixer (Chunk, Music)
import Data.Hashable (Hashable)


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
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Fractional, Floating, Real, RealFrac, Hashable)

data World = World
  { w_levels :: Map Text Level
  }

data CollisionPurpose
  = CollisionWall
  | CollisionGround
  | CollisionCeiling
  | CollisionCheckGround
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

data Level = Level
  { l_bgcolor :: Color
  , l_tilebounds :: Rect Tile
  , l_bounds  :: Rect Pixel
  , l_tiles  :: LevelLayer -> Renderable
  , l_hitmap :: LevelLayer -> CollisionPurpose -> V2 Tile -> Bool
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
  , r_songs    :: Song -> Music
  , r_worlds   :: WorldName -> World
  , r_sprites  :: Sprite -> Anim -> [WrappedTexture]
  , r_glyphs   :: Char -> Texture
  }


------------------------------------------------------------------------------

type Color = V4 Word8

type Renderable = Camera -> IO ()


------------------------------------------------------------------------------
-- | Things that change every frame.
data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  , fi_global :: ~GlobalState
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
    | ChickenTexture
    | Parallax0
    | Parallax1
    | Parallax2
    | ChargeTexture
    | TeleTexture
    | AuraTexture
    | TrampolineTexture
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Tileset
  -- NOTE: It's important that the tileset names line up with their png names,
  -- so levels can import them properly.
  = Cavernas_by_Adam_Saltsman
  | Stringstar_Fields
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data WorldName = TestWorld | HelpWorld
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------------------------
-- | Audio used by the game.
data Sound
    = NintendoSound
    | CheckpointSound
    | CoinSound
    | DieSound
    | JumpSound
    | StepSound
    | ThudSound
    | WarpSound
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Song
  = WarmDuckShuffle
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
  , c_z :: Bool
  , c_reset :: Bool
  , c_dir :: V2 Int
  }
  deriving (Eq)

defaultControls :: Controls
defaultControls = Controls
  { c_space = False
  , c_reset = False
  , c_z = False
  , c_dir = V2 0 0
  }

tileSize :: Num a => a
tileSize = 16


------------------------------------------------------------------------------

newtype ObjectId = ObjectId
  { getObjectId :: Int
  }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Enum, Bounded)

type ObjSF = SF ObjectInput ObjectOutput

type Object = ObjSF

data PowerupType
    = PowerupDoubleJump
    | PowerupWarpBall
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

data ObjectTag
    = IsPlayer
    | IsPowerup PowerupType
    | HasPowerup PowerupType
  deriving (Eq, Ord, Show, Generic)

data ObjectMeta = ObjectMeta
  deriving stock (Eq, Ord, Show, Generic)

noObjectMeta :: ObjectMeta
noObjectMeta = ObjectMeta

type HitEvent = (ObjectId, ObjectState)

data ObjectInput = ObjectInput
  { oi_self :: ObjectId
  , oi_events :: ObjectInEvents
  , oi_frameInfo :: FrameInfo
  , oi_state :: ObjectState
  }
  deriving stock Generic

data ObjectInEvents = ObjectInEvents
  { oie_hit :: Event [HitEvent]
  , oie_receive :: Event [(ObjectId, Message)]
  }
  deriving stock Generic

instance Semigroup ObjectInEvents where
  (ObjectInEvents ev ev') <> (ObjectInEvents ev2 ev3)
    = ObjectInEvents {oie_hit = ev <> ev2, oie_receive = ev' <> ev3}

instance Monoid ObjectInEvents where
  mempty = ObjectInEvents {oie_hit = mempty, oie_receive = mempty}

data GlobalState = GlobalState
  { gs_currentLevel :: ~Level
  , gs_layerset :: Set LevelLayer
  }
  deriving stock Generic

data ObjectEvents = ObjectEvents
  { oe_die :: Event ()
  , oe_spawn :: Event [Object]
  , oe_focus :: Event ()
  , oe_play_sound :: Event [Sound]
  , oe_send_message :: Event [(ObjectId, Message)]
  , oe_omnipotence :: Event (ObjectMap ObjSF -> ObjectMap ObjSF )
  , oe_broadcast_message :: Event [Message]
  }
  deriving stock Generic

instance Semigroup ObjectEvents where
  (ObjectEvents ev ev' ev2 ev3 sm1 ev4 bc1) <> (ObjectEvents ev5 ev6 ev7 ev8 sm2 ev9 bc2)
    = ObjectEvents
        { oe_die = ev <> ev5
        , oe_spawn = ev' <> ev6
        , oe_focus = ev2 <> ev7
        , oe_play_sound = ev3 <> ev8
        , oe_send_message = sm1 <> sm2
        , oe_omnipotence = fmap appEndo $ coerce ev4 <> coerce ev9
        , oe_broadcast_message = bc1 <> bc2
        }

instance Monoid ObjectEvents where
  mempty
    = ObjectEvents
        { oe_die = mempty
        , oe_spawn = mempty
        , oe_focus = mempty
        , oe_play_sound = mempty
        , oe_send_message = mempty
        , oe_omnipotence = fmap appEndo mempty
        , oe_broadcast_message = mempty
        }

data ObjectState = ObjectState
  { os_pos :: V2 WorldPos
  , os_collision :: Maybe (OriginRect Double)
  , os_tags :: Set ObjectTag
  , os_camera_offset :: V2 Double
  }
  deriving stock (Eq, Ord, Show, Generic)

data ObjectOutput = ObjectOutput
  { oo_events :: ObjectEvents
  , oo_render :: Renderable
  , oo_state :: ObjectState
  }
  deriving stock Generic

data ObjectMap a = ObjectMap
  { objm_camera_focus :: ObjectId
  , objm_undeliveredMsgs :: Map ObjectId [(ObjectId, Message)]
  , objm_globalState :: ~GlobalState
  , objm_map :: Map ObjectId a
  }
  deriving stock (Functor, Generic, Foldable)

data OriginRect aff = OriginRect
  { orect_size   :: V2 aff
  , orect_offset :: V2 aff
  }
  deriving (Eq, Ord, Show, Functor, Generic)

newtype Camera = Camera (V2 WorldPos)

instance Semigroup Camera where
  (Camera v2) <> (Camera v2') = Camera $ v2 + v2'

logicalSize :: Num a => V2 a
logicalSize = V2 320 240

screenRect :: (Fractional a) => Rectangle a
screenRect = Rectangle (P $ -tileSize * buffer) (logicalSize + tileSize * 2 * buffer)
  where
    buffer = 4

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

data DrawSpriteDetails = DrawSpriteDetails
  { dsd_anim :: Anim
  , dsd_rotation :: Double
  , dsd_flips :: V2 Bool
  }
  deriving stock (Eq, Ord, Show, Read, Generic)


data Message
  = TeleportTo (V2 WorldPos)
  | TeleportOpportunity (V2 WorldPos)
  | SetCheckpoint (V2 WorldPos)
  | OnTrampoline Double
  | Die
  | CurrentCheckpoint ObjectId
  deriving stock (Eq, Ord, Show, Read, Generic)

data ParticleType
  = Gore
  | Firework
  deriving stock (Eq, Ord, Show, Read, Generic)

traceF :: Show b => (a -> b) -> a -> a
traceF f a = trace (show $ f a) a

traceFX :: Show b => String -> (a -> b) -> a -> a
traceFX herald f a = trace (mappend (herald <> ": ") . show $ f a) a

------------------------------------------------------------------------------
  --
class HasFrameInfo a where
  frameInfo :: a -> FrameInfo

instance HasFrameInfo ObjectInput where
  frameInfo = oi_frameInfo

class HasDeltaTime a where
  deltaTime :: a -> Time

instance HasDeltaTime FrameInfo where
  deltaTime = fi_dt

instance HasDeltaTime RawFrameInfo where
  deltaTime = rfi_dt

instance {-# OVERLAPPABLE #-} HasFrameInfo a => HasDeltaTime a where
  deltaTime = deltaTime . frameInfo

class HasGlobalState a where
  globalState :: a -> GlobalState

instance HasGlobalState FrameInfo where
  globalState = fi_global

instance {-# OVERLAPPABLE #-} HasFrameInfo a => HasGlobalState a where
  globalState = globalState . frameInfo

class HasControls a where
  controls :: a -> Controls

instance HasControls FrameInfo where
  controls = fi_controls

instance HasControls RawFrameInfo where
  controls = rfi_controls

instance {-# OVERLAPPABLE #-} HasFrameInfo a => HasControls a where
  controls = controls . frameInfo

