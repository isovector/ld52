module Engine.CoreTypes
  ( module Engine.CoreTypes
  , module SDL
  ) where

import SDL (V2(..), Rectangle(..), Point(..))
import Data.Hashable (Hashable)
import Data.Text (Text)


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


data ObjectId
  = StaticId Text
  | DynamicId Int
  deriving stock (Show, Read)
  deriving stock (Eq, Ord)

