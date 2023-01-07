module Game where

import           Control.Lens ((^.))
import           Control.Monad (void)
import           Data.Bool (bool)
import           Data.Foldable (fold)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects
import           Game.World (drawWorld)
import           SDL
import           SDL.Mixer
import           Types
import Resources (loadResources)

nowish :: a -> SF x (Types.Event a)
nowish a = after 0.016 a

shrapnel :: Int -> V2 Double -> Double -> Object
shrapnel n pos0 theta = arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- never -< () -- after 2 () -< ()
    focus <- after (fromIntegral n) () -< ()
    let dt = fi_dt fi
    let pos' = pos + V2 (cos theta) (sin theta) ^* 50 ^* dt
    returnA -<
      ( ObjectOutput
          { oo_events = ObjectEvents die noEvent focus
          , oo_render
              = drawFilledRect (V4 255 0 0 255)
              $ flip Rectangle 3
              $ P
              $ fmap round pos'
          , oo_pos = pos'
          }
      , pos'
      )
  )


grenade :: Object
grenade =
  timedSequence
    (proc _ -> do
      die <- after 3 () -< ()
      sp <- now () -< ()
      returnA -<
         ObjectOutput (ObjectEvents
            die
            (tag sp $ do
              n <- [id @Int 0 .. 5]
              pure $ shrapnel n pos $ 2 * pi / 6 * fromIntegral n
            )
            noEvent
            )
            (drawFilledRect (V4 255 0 0 255) $ flip Rectangle 8 $ fmap round $ P pos)
            pos

    ) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput
            (ObjectEvents noEvent noEvent noEvent)
            (drawFilledRect col $ flip Rectangle 8 $ fmap round $ P pos)
            pos
  where
    pos = V2 50 50


playSound :: Sound -> Resources -> IO ()
playSound s r = do
  putStrLn "hi"
  halt 0
  void $ playOn 0 Once $ r_sounds r s

data Player = Player
  { p_pos :: V2 Double
  , p_vel :: V2 Double
  }

game :: Resources -> SF FrameInfo ScreenRenderable
game rs
  = fmap fold
  $ par (\fi -> fmap (fi, ))
  $ thingsToRunAtOnce rs

thingsToRunAtOnce :: Resources -> [SF FrameInfo ScreenRenderable]
thingsToRunAtOnce rs =
  [ -- game5 rs
    -- game4 rs
    renderObjects (V2 0 0) $ addObject grenade $ ObjectMap (ObjectId 0) mempty
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

