module Controls where

import Types
import qualified SDL.Input as SDL


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls = pure Controls

