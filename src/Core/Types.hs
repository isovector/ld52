module Core.Types
  ( module Core.Types
  , module SDL
  ) where

import SDL (V2(..), Rectangle(..), Point(..))
import Data.Hashable (Hashable)


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


newtype ObjectId = ObjectId
  { getObjectId :: Int
  }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Enum, Bounded)

