{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE CPP #-}
module Game where

import           Data.Map (toList)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.World (drawWorld)
import           SDL
import           Types
import Control.Lens hiding (Level)

#ifndef __HLINT__

nowish :: a -> SF x (Types.Event a)
nowish a = after 0.016 a

shrapnel :: Int -> V2 WorldPos -> Double -> Object
shrapnel _n pos0 theta = Object noObjectMeta $ arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- never -< () -- after 2 () -< ()
    let dt = fi_dt fi
    let pos' = pos + coerce (V2 (cos theta) (sin theta) ^* 50 ^* dt)
    returnA -<
      ( ObjectOutput
          { oo_events = ObjectEvents die noEvent noEvent noEvent
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
grenade = Object noObjectMeta $
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
            ([NintendoSound] <$ die)
            )
            (drawFilledRect (V4 255 0 0 255) $ flip Rectangle 8 $ P pos)
            pos

    ) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput
            (ObjectEvents noEvent noEvent noEvent noEvent)
            (drawFilledRect col $ flip Rectangle 8 $ P pos)
            pos
  where
    pos = V2 50 50

data Player = Player
  { p_pos :: V2 WorldPos
  , p_vel :: V2 Double
  } deriving Show

initialObjs :: Resources -> ObjectMap ObjSF
initialObjs rs
  = addObject (player rs)
  $ addObject grenade
  $ ObjectMap (ObjectId 0) mempty

player :: Resources -> Object
player rs = Object noObjectMeta $ game4 rs


game :: Resources -> SF FrameInfo (Camera, Renderable)
game rs = proc fi -> do
  (cam, objs) <- renderObjects rs (V2 0 0) (initialObjs rs) -< fi
  bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
  returnA -< (cam, bg <> objs)


game4 :: Resources -> SF ObjectInput ObjectOutput
game4 rs =
  do
  loopPre (Player zero zero) $ proc (ObjectInput hit fi, Player pos vel) -> do
    
    focus <- nowish () -< ()
    let dt = fi_dt fi

    let grav = V2 0 10
    let jumpVel = V2 0 (-200)
    let stepSpeed = 2
    jumpEv <- edge -< c_space (fi_controls fi) -- TODO: Only jump when on the ground
    let jump = event zero (const jumpVel) jumpEv
    let vx = vel * V2 1 0 + V2 stepSpeed 0 * (realToFrac <$> c_dir (fi_controls fi))
    let vy = V2 0 1 * vel + grav + jump
    let vel' = vx + vy

    let (_name, lev) = head $ toList $ w_levels $ r_worlds rs TestWorld
    -- let hits = hitTiles lev Layer1 pos'
    -- let player' = if or hits then collide lev Layer1 (p_pos p) pos' else Player pos' vel'

    let dpos = dt SDL.*^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move (l_hitmap lev Layer1 . posToTile) 7 pos $ dpos

    let vel'' = V2 (if desiredPos ^. _x == pos' ^. _x then vel' ^. _x else 0) (if desiredPos ^. _y == pos' ^. _y then vel' ^. _y else 0)
    let player' = Player pos' vel''

    -- returnA -< ((p_pos player', drawFilledRect (event (V4 255 0 0 255) (const $ V4 255 255 0 255) hit) $ Rectangle (P (p_pos player' - 3.5)) 7), player')
    returnA -< (ObjectOutput {
      oo_events = ObjectEvents
      {oe_die = noEvent, oe_spawn = noEvent, oe_focus = focus, oe_play_sound = noEvent},
      oo_render = drawFilledRect (event (V4 255 0 0 255) (const $ V4 255 255 0 255) hit) $ Rectangle (P (p_pos player' - 3.5)) 7, 
      oo_pos = pos'
      }, 
      player')

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

hitTile :: (V2 Tile -> Bool) -> V2 WorldPos -> Bool
hitTile f = f . posToTile

cornersX :: V2 Double -> Int -> V2 WorldPos -> (V2 WorldPos, V2 WorldPos)
cornersX ((/ 2) -> V2 (coerce -> sx) sy) ydir p =
  let sy' :: WorldPos
      sy' = coerce $ sy * fromIntegral ydir
   in (p + V2 (-sx) sy', p + V2 sx sy')

cornersY :: V2 Double -> Int -> V2 WorldPos -> (V2 WorldPos, V2 WorldPos)
cornersY ((/ 2) -> V2 sx (coerce -> sy)) xdir p =
  let sx' :: WorldPos
      sx' = coerce $ sx * fromIntegral xdir
   in (p + V2 sx' (-sy), p + V2 sx' sy)

move :: (V2 WorldPos -> Bool) -> V2 Double -> V2 WorldPos -> V2 Double -> V2 WorldPos
move f sz pos dpos =
  let (V2 xd yd) = fmap (round @_ @Int) $ signum dpos
   in  moveX f sz xd (moveY f sz yd (pos + coerce dpos))

epsilon :: Fractional a => a
epsilon = 0.01

moveX :: (V2 WorldPos -> Bool) -> V2 Double -> Int -> V2 WorldPos -> V2 WorldPos
moveX f sz xdir pos =
  let (l, r) = cornersY sz xdir pos
   in case f l || f r of
        False -> pos
        True ->
          case xdir of
            -1 -> pos & _x .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2) ^. _x)
            0 -> pos -- already in the wall
            1 -> pos & _x .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2 - epsilon) ^. _x)
            _ -> error "very impossible"

moveY :: (V2 WorldPos -> Bool) -> V2 Double -> Int -> V2 WorldPos -> V2 WorldPos
moveY f sz ydir pos =
  let (l, r) = cornersX sz ydir pos
   in case f l || f r of
        False -> pos
        True ->
          case ydir of
            -1 -> pos & _y .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2) ^. _y)
            0 -> pos -- already in the wall
            1 -> pos & _y .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2 - epsilon) ^. _y)
            _ -> error "very impossible"

tileToPos :: V2 Tile -> V2 WorldPos
tileToPos = fmap (WorldPos . fromIntegral . getTile) . (* tileSize)

#endif
