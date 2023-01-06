module Controls where

import Types
import qualified SDL.Input as SDL
import SDL.Input.Keyboard.Codes


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls check = let toOne code = if check code then 1 else 0 in
  Controls { c_space = check ScancodeSpace
  , c_dir = V2 (toOne ScancodeRight - toOne ScancodeLeft) (toOne ScancodeUp - toOne ScancodeDown)
  }

