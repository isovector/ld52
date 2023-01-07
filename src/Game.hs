module Game where

import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad (void)
import Data.Bool (bool)
import FRP
import Game.World (drawWorld)
import SDL
import SDL.Mixer
import Types
import Drawing
import Control.Lens ((^.))
import Data.Foldable (fold)
import Game.Objects (renderObjects, addObject)

shrapnel :: V2 Double -> Double -> Object
shrapnel pos0 theta = arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- after 2 () -< ()
    let dt = fi_dt fi
    let pos' = pos + V2 (cos theta) (sin theta) ^* 50 ^* dt
    returnA -<
      ( ObjectOutput
          { oo_die = die
          , oo_spawn = noEvent
          , oo_render
              = drawFilledRect (V4 255 0 0 255)
              $ flip Rectangle 3
              $ P
              $ fmap round pos'
          }
      , pos'
      )
  )


grenade :: Object
grenade =
  timedSequence
    (arr $ const
         $ ObjectOutput
            (FRP.Event ())
            (FRP.Event $ do
              n <- [id @Int 0 .. 5]
              pure $ shrapnel pos $ traceShowId (2 * pi / 6 * fromIntegral n)
            )
            mempty) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput noEvent noEvent
        $ drawFilledRect col $ flip Rectangle 8 $ fmap round $ P pos
  where
    pos = V2 50 50



logicalSize :: Num a => V2 a
logicalSize = V2 320 240

playSound :: Sound -> Resources -> IO ()
playSound s r = do
  putStrLn "hi"
  halt 0
  void $ playOn 0 Once $ r_sounds r s

data Player = Player
  { p_pos :: V2 Double
  , p_vel :: V2 Double
  }

game :: Resources -> SF FrameInfo Renderable
game rs
  = fmap fold
  $ par (\fi -> fmap (fi, ))
  $ thingsToRunAtOnce rs

thingsToRunAtOnce :: Resources -> [SF FrameInfo Renderable]
thingsToRunAtOnce rs =
  -- [ game5 rs
  -- , game4 rs
  [ renderObjects $ addObject grenade mempty
  ]

game5 :: Resources -> SF i Renderable
game5 rs = timedSequence undefined 1 $ cycle $
  [ arr $ const $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld
  , arr $ const $ drawWorld rs (S.singleton Layer2) $ r_worlds rs TestWorld
  , arr $ const $ drawWorld rs (S.fromList [Layer1, Layer2]) $ r_worlds rs TestWorld
  ]

game4 :: Resources -> SF FrameInfo Renderable
game4 rs =
  do
  loopPre (Player (V2 150 150) zero) $ proc (fi, p) -> do
    let dt = fi_dt fi
    let grav = V2 0 0.1
    let jumpPower = V2 0 (-5)
    jumpEv <- edge -< c_space (fi_controls fi)
    let jump = event zero (const jumpPower) jumpEv --if c_space (fi_controls fi) then V2 0 (-1) else V2 0 0
    let hvel = 2 SDL.*^ V2 1 0 * (realToFrac <$> c_dir (fi_controls fi))
    let vvel = p_vel p + grav + jump
    let vel' = hvel + V2 0 1 * vvel
    let pos' = p_pos p + vel'
    let (pos'', vel'') = (if pos' ^. _y > 200 then (V2 (pos' ^. _x) 200, V2 (vel' ^. _x) 0) else (pos', vel'))

    returnA -< (drawFilledRect (V4 255 0 0 255) $ round <$> Rectangle (P pos') 16, Player pos'' vel'')

