{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Controls (parseControls)
import Data.IORef
import Data.Time.Clock.System
import FRP.Yampa
import Resources (loadResources)
import SDL hiding (copy, Stereo)
import SDL.Mixer hiding (quit)
import System.Exit
import Types


screenSize :: Num a => V2 a
screenSize = V2 640 480

screenScale :: Num a => V2 a
screenScale = V2 2 2

game :: Resources -> SF FrameInfo Renderable
game _ = pure $ pure $ pure ()

main :: IO ()
main = do
  initializeAll

  window <- createWindow "ld50" $ defaultWindow
    { windowInitialSize = screenSize
    , windowGraphicsContext = OpenGLContext defaultOpenGL
    }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedVSyncRenderer
    , rendererTargetTexture = True
    }
  rendererScale renderer $= screenScale
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
  rs <- loadResources engine

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo defaultControls 0.016)
    (input window tRef)
    (output rs)
    (game rs)
  quit




input :: Window -> IORef Double -> Bool -> IO (Double, Maybe FrameInfo)
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

  keys <- getKeyboardState

  let dt = min 0.016 (seconds' - seconds)
  pure (dt, Just $ FrameInfo (parseControls keys) dt)

isQuit :: EventPayload -> Bool
isQuit QuitEvent             = True
isQuit (WindowClosedEvent _) = True
isQuit _                     = False


output :: Resources -> Bool -> Renderable -> IO Bool
output rs _ render = do
  let e = r_engine rs
      renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render rs
  present renderer
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9

