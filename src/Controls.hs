module Controls where

import Types
import qualified SDL.Input as SDL
import SDL.Input.Keyboard.Codes


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls check = Controls
  { c_space = check ScancodeSpace
  }

