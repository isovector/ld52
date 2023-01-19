module Game.Controls where

import           Data.Int (Int16)
import           Engine.Types
import           SDL (Joystick, buttonPressed, axisPosition)
import qualified SDL.Input as SDL
import           SDL.Input.GameController (ControllerButton(..))
import           SDL.Input.Keyboard.Codes
import           SDL.Internal.Numbered (toNumber)

controllerButton :: Joystick -> ControllerButton -> IO Bool
controllerButton js = buttonPressed js . fromIntegral . toNumber

parseController :: Joystick -> IO Controls
parseController js = do
  a <- controllerButton js ControllerButtonA
  b <- controllerButton js ControllerButtonB
  x <- controllerButton js ControllerButtonX
  y <- controllerButton js ControllerButtonY
  l <- controllerButton js ControllerButtonBack
  r <- controllerButton js ControllerButtonGuide

  dx <- axisPosition js 0
  dy <- axisPosition js 1
  pure $ Controls
    { c_space = a || x
    , c_z = y
    , c_c = b
    , c_reset = r
    , c_full_restart = l
    , c_dir = fmap clampAxis $ V2 dx dy
    }

clampAxis :: Int16 -> Int
clampAxis (fromIntegral @_ @Int -> i) =
  if abs i <= 8000
     then 0
     else signum i


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls check = Controls
  { c_space = check ScancodeX || check ScancodeSpace
  , c_z = check ScancodeZ || check ScancodeReturn
  , c_c = check ScancodeC
  , c_reset = check ScancodeR && not (check ScancodeLShift || check ScancodeRShift)
  , c_full_restart = check ScancodeR && (check ScancodeLShift || check ScancodeRShift)
  , c_dir =
      V2
        (toOne ScancodeRight - toOne ScancodeLeft)
        (toOne ScancodeDown - toOne ScancodeUp)
  }
  where
    toOne code = if check code then 1 else 0

