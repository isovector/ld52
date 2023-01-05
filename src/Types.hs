module Types
  ( module Types
  , V2 (..)
  , Generic
  , Word8
  ) where

import SDL
import GHC.Generics
import Data.Word


------------------------------------------------------------------------------

data Engine = Engine
  { e_renderer :: Renderer
  , e_window :: Window
  }

------------------------------------------------------------------------------
-- | Things we need to keep track of, like sprites and music and stuff.
data Resources = Resources
  { r_engine :: Engine
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
-- | Input for the frame.
data Controls = Controls
  { c_space :: Bool
  }

defaultControls :: Controls
defaultControls = Controls
  { c_space = False
  }

