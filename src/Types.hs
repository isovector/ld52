module Types
  ( module Types
  , Generic
  ) where

import SDL
import GHC.Generics


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

defaultControls :: Controls
defaultControls = Controls

