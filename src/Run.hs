{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Run where

import Control.Monad
import Controls (parseControls)
import Data.IORef
import Data.Time.Clock.System
import FRP.Yampa
import Game
import SDL hiding (copy, Stereo)
import SDL.Mixer hiding (quit)
import System.Exit
import Types
import Globals (veryUnsafeEngineIORef, global_resources)
import Game.Splash (mainMenu, runIntro)


screenSize :: Num a => V2 a
screenSize = V2 640 480


main :: IO ()
main = do
  initializeAll

  window <- createWindow "ld52" $ defaultWindow
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

  openAudio
    (Audio
      { audioFrequency = 44100
      , audioFormat = FormatS16_Sys
      , audioOutput = Stereo
      })
      1024

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
    (pure $ RawFrameInfo defaultControls 0.016)
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
    haltMusic
    destroyWindow win
    exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  keys <- getKeyboardState

  let dt = min 0.016 (seconds' - seconds)
  pure (dt, Just $ RawFrameInfo (parseControls keys) dt)


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

