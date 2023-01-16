module Game.Controls where

import           Engine.Types
import qualified SDL.Input as SDL
import           SDL.Input.Keyboard.Codes


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls check = Controls
  { c_space = check ScancodeX || check ScancodeSpace
  , c_z = check ScancodeZ
  , c_c = check ScancodeC
  , c_reset = check ScancodeR
  , c_dir =
      V2
        (toOne ScancodeRight - toOne ScancodeLeft)
        (toOne ScancodeDown - toOne ScancodeUp)
  }
  where
    toOne code = if check code then 1 else 0

