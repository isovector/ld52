module Game.Types where

import Core.Types
import Data.Set (Set)
import GHC.Generics (Generic)


data GameState = GameState
  { gs_coins :: Int
  , gs_inventory :: Set PowerupType
  }
  deriving stock Generic

data GameMessage
  = AddCoin
  | AddInventory
  deriving stock (Eq, Ord, Enum, Bounded, Show, Read, Generic)

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

data PowerupType
    = PowerupDoubleJump
    | PowerupWarpBall
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

data ObjectTag
    = IsPlayer
    | IsPowerup PowerupType
    | HasPowerup PowerupType
  deriving (Eq, Ord, Show, Generic)

logicalSize :: Num a => V2 a
logicalSize = V2 320 240

screenRect :: (Fractional a) => Rectangle a
screenRect = Rectangle (P $ -tileSize * buffer) (logicalSize + tileSize * 2 * buffer)
  where
    buffer = 4

data Sprite
  = MainCharacter
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)


data Anim
  = Idle
  | NoAnim
  | Run
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

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

