{-# LANGUAGE CPP #-}

module Game where

import           Collision (move)
import           Data.Bool (bool)
import qualified Data.Set as S
import           Drawing
import           FRP hiding ((*^))
import           Game.Objects (renderObjects, addObject)
import           Game.World (drawWorld)
import           SDL
import           Types
import           Utils
import Data.Foldable (toList)

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
player rs
  = Object noObjectMeta
  $ arr (head $ toList $ w_levels $ r_worlds rs TestWorld ,) >>> game4 7 (drawPlayer rs 7)


drawPlayer :: Resources -> V2 Double -> SF (ObjectInput, V2 WorldPos) Renderable
drawPlayer rs sz = arr $ \(_, pos) ->
  drawSprite
    (setGroundOrigin $ r_textures rs MainCharacter)
    (pos - coerce sz / 2)
    0
    (V2 False False)



game :: Resources -> SF FrameInfo (Camera, Renderable)
game rs = proc fi -> do
  (cam, objs) <- renderObjects rs (V2 0 0) (initialObjs rs) -< fi
  bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
  returnA -< (cam, bg <> objs)


playerPhysVelocity :: SF FrameInfo (V2 Double)
playerPhysVelocity = proc fi -> do
  let jumpVel = V2 0 (-200)
  let stepSpeed = 2
  jumpEv <- edge -< c_space (fi_controls fi) -- TODO: Only jump when on the ground
  let jump = event zero (const jumpVel) jumpEv
  let vx = V2 stepSpeed 0 * (realToFrac <$> c_dir (fi_controls fi))
  let vy = jump
  let vel' = vx + vy
  returnA -< vel'


game4
    :: V2 Double
    -> SF (ObjectInput, V2 WorldPos) Renderable
    -> SF (Level, ObjectInput) ObjectOutput
game4 sz render = loopPre (Player zero zero) $
  proc ((lev, oi@(ObjectInput _ fi)), Player pos vel) -> do
    let dt = fi_dt fi

    focus <- nowish () -< ()
    vel'0 <- playerPhysVelocity -< fi

    let grav = V2 0 10
    let vel' = vel + vel'0 + grav

    let dpos = dt *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move (l_hitmap lev Layer1 . posToTile) sz pos $ dpos

    let vel''
          = (\want have res -> bool 0 res $ want == have)
              <$> desiredPos
              <*> pos'
              <*> vel'

    let player' = Player pos' vel''
    img <- render -< (oi, pos')

    returnA -<
      ( ObjectOutput
        { oo_events = mempty { oe_focus = focus }
        , oo_render = img
        , oo_pos = pos'
        }
      , player'
      )

#endif
