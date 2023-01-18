{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Engine.Run where

import           Control.Monad
import           Data.IORef
import           Data.Time.Clock.System
import           Engine.Globals (veryUnsafeEngineIORef, global_resources)
import           Engine.Prelude
import           Game.Controls (parseControls)
import           Game.Splash (runIntro)
import           SDL hiding (copy, Stereo)
import qualified Sound.ALUT as ALUT
import           System.Exit


screenSize :: Num a => V2 a
screenSize = V2 640 480


main :: IO ()
main = ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  initializeAll

  window <- createWindow "Where's My Chicken, Man?" $ defaultWindow
    { windowInitialSize = screenSize
    , windowGraphicsContext = OpenGLContext defaultOpenGL
    }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedVSyncRenderer
    , rendererTargetTexture = True
    }
  rendererScale renderer $= screenSize / logicalSize
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  let engine = Engine
        { e_renderer = renderer
        , e_window = window
        }
  !() <- writeIORef veryUnsafeEngineIORef engine
  !rs <- pure global_resources

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo defaultControls 0.016 ())
    (input window tRef)
    (output rs)
    -- game
    runIntro
  quit


input :: Window -> IORef Double -> Bool -> IO (Double, Maybe RawFrameInfo)
input win tRef _ = do
  pumpEvents
  es <- pollEvents
  when (any (isQuit . eventPayload) es) $ do
    destroyWindow win
    exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  let dt = seconds' - seconds

  keys <- getKeyboardState
  pure (dt, Just $ FrameInfo (parseControls keys) dt ())


pattern Keypress :: Scancode -> EventPayload
pattern Keypress scan <- KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym scan _ _))


isQuit :: EventPayload -> Bool
isQuit QuitEvent                   = True
isQuit (WindowClosedEvent _)       = True
isQuit (Keypress ScancodeEscape)   = True
isQuit (Keypress ScancodeCapsLock) = True
isQuit _                           = False


output :: Resources -> Bool -> (Camera, Renderable) -> IO Bool
output rs _ (cam, render) = do
  let e = r_engine rs
      renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render cam
  present renderer
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9

