module Game where

import qualified Data.Set as S
import Control.Monad (void)
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
  { p_pos :: V2 Pos
  , p_vel :: V2 Double
  } deriving Show

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
  -- , arr $ const $ drawWorld rs (S.singleton Layer2) $ r_worlds rs TestWorld
  -- , arr $ const $ drawWorld rs (S.fromList [Layer1, Layer2]) $ r_worlds rs TestWorld
  ]

game4 :: Resources -> SF FrameInfo Renderable
game4 rs =
  do
  loopPre (Player zero zero) $ proc (fi, p) -> do
    let dt = fi_dt fi
    let grav = V2 0 10
    let jumpVel = V2 0 (-300)
    let stepSpeed = 200
    jumpEv <- edge -< c_space (fi_controls fi) -- Only jump when on the ground
    let jump = event zero (const jumpVel) jumpEv
    let hvel = stepSpeed SDL.*^ V2 1 0 * (realToFrac <$> c_dir (fi_controls fi))
    let vvel = p_vel p + grav + jump
    let vel' = hvel + V2 0 1 * vvel
    let pos' = p_pos p + (Pos <$> dt SDL.*^ vel')

    let lev = head $ toList $ w_levels $ r_worlds rs TestWorld
    let hits = hitTiles lev Layer1 pos'
    let player' = if or hits then collide lev Layer1 (p_pos p) pos' else Player pos' vel'

    returnA -< (drawFilledRect (V4 255 0 0 255) $ round . getPos <$> Rectangle (P (p_pos player')) 8, player')

posToTile :: V2 Pos -> V2 Tile
posToTile = fmap $ Tile . floor . (/8) . getPos

collide :: Level -> LevelLayer -> V2 Pos -> V2 Pos -> Player
collide lev layer pos0 pos1 = let
  n = 4
  subVels = replicate n $ (1 / realToFrac n) SDL.*^ fmap getPos (pos1 - pos0)
  dPos = fmap Pos <$> zipWith (SDL.*^) (fmap realToFrac [1 .. n]) subVels
  subPos = zipWith (+) (replicate n pos0) dPos

  validPos = filter (not . or . hitTiles lev layer) subPos
  pos' = case validPos of
    [] -> pos0
    _ -> last validPos

  in Player pos' zero --TODO: Only set vertical or horizontal to zero

-- TODO: This doesn't make sense if the character isn't exactly the size of a tile. Each corner needs calculating
hitTiles :: Level -> LevelLayer -> V2 Pos -> [Bool]
hitTiles lev layer pos = l_hitmap lev layer <$> zipWith (+) (replicate 4 (posToTile pos)) [V2 0 0, V2 1 0, V2 0 1, V2 1 1]