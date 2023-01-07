module Game where

import           Control.Monad (void)
import           Data.Foldable (fold)
import           Data.Map (toList)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.World (drawWorld)
import           SDL
import           SDL.Mixer
import           Types

nowish :: a -> SF x (Types.Event a)
nowish a = after 0.016 a

shrapnel :: Int -> V2 WorldPos -> Double -> Object
shrapnel n pos0 theta = arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- never -< () -- after 2 () -< ()
    focus <- after (fromIntegral n) () -< ()
    let dt = fi_dt fi
    let pos' = pos + coerce (V2 (cos theta) (sin theta) ^* 50 ^* dt)
    returnA -<
      ( ObjectOutput
          { oo_events = ObjectEvents die noEvent focus
          , oo_render
              = drawFilledRect (V4 255 0 0 255)
              $ flip Rectangle 3
              $ P pos'
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
            (drawFilledRect (V4 255 0 0 255) $ flip Rectangle 8 $ P pos)
            pos

    ) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput
            (ObjectEvents noEvent noEvent noEvent)
            (drawFilledRect col $ flip Rectangle 8 $ P pos)
            pos
  where
    pos = V2 50 50


playSound :: Sound -> Resources -> IO ()
playSound s r = do
  putStrLn "hi"
  halt 0
  void $ playOn 0 Once $ r_sounds r s

data Player = Player
  { p_pos :: V2 WorldPos
  , p_vel :: V2 Double
  } deriving Show

game :: Resources -> SF FrameInfo ScreenRenderable
game rs
  = fmap fold
  $ par (\fi -> fmap (fi, ))
  $ thingsToRunAtOnce rs

thingsToRunAtOnce :: Resources -> [SF FrameInfo ScreenRenderable]
thingsToRunAtOnce rs =
  [ game5 rs >>> arr ($ V2 0 0)
  , game4 rs >>> arr ($ V2 0 0)
  , renderObjects (V2 0 0) $ addObject grenade $ ObjectMap (ObjectId 0) mempty
  ]

game5 :: Resources -> SF i Renderable
game5 rs = timedSequence undefined 1 $ cycle $
  [ arr $ const $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld
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
    let pos' = p_pos p + (WorldPos <$> dt SDL.*^ vel')

    let (_name, lev) = head $ toList $ w_levels $ r_worlds rs TestWorld
    let hits = hitTiles lev Layer1 pos'
    let player' = if or hits then collide lev Layer1 (p_pos p) pos' else Player pos' vel'

    returnA -< (drawFilledRect (V4 255 0 0 255) $ Rectangle (P (p_pos player')) 8, player')

posToTile :: V2 WorldPos -> V2 Tile
posToTile = fmap $ Tile . floor . (/8) . getWorldPos

collide :: Level -> LevelLayer -> V2 WorldPos -> V2 WorldPos -> Player
collide lev layer pos0 pos1 = let
  n = 4
  subVels = replicate n $ (1 / realToFrac n) SDL.*^ fmap getWorldPos (pos1 - pos0)
  dPos = fmap WorldPos <$> zipWith (SDL.*^) (fmap realToFrac [1 .. n]) subVels
  subPos = zipWith (+) (replicate n pos0) dPos

  validPos = filter (not . or . hitTiles lev layer) subPos
  pos' = case validPos of
    [] -> pos0
    _ -> last validPos

  in Player pos' zero --TODO: Only set vertical or horizontal to zero

-- TODO: This doesn't make sense if the character isn't exactly the size of a tile. Each corner needs calculating
hitTiles :: Level -> LevelLayer -> V2 WorldPos -> [Bool]
hitTiles lev layer pos = l_hitmap lev layer <$> zipWith (+) (replicate 4 (posToTile pos)) [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
