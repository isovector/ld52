module Game.Objects.Actor where

import Types
import FRP
import Collision
import Utils
import Data.Bool (bool)
import Data.Monoid


actor
    :: OriginRect WorldPos
    -> SF (FrameInfo) (V2 Double)
    -> SF (ObjectInput, V2 WorldPos) Renderable
    -> V2 WorldPos
    -> SF (Bool, ObjectInput) ObjectOutput
actor ore input render pos0 = loopPre (pos0, 0) $
  proc ((can_double, oi@(ObjectInput _ fi os)), (pos, vel)) -> do
    let dt = fi_dt fi
    let lev = gs_currentLevel $ fi_global fi
        layers = gs_layerset $ fi_global fi

        collision purpose = (getAny . foldMap ((fmap Any .) . l_hitmap lev) layers purpose . posToTile)

    vel'0 <- input -< fi

    let onGround = touchingGround (collision CollisionCheckGround) ore pos
    let vel' = updateVel (can_double || onGround) vel vel'0
    let dpos = dt *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move collision (coerce ore) pos $ dpos

    let vel''
          = (\want have res -> bool 0 res $ abs(want - have) <= epsilon )
              <$> desiredPos
              <*> pos'
              <*> vel'

    img <- render -< (oi, pos')

    returnA -<
      ( ObjectOutput
        { oo_events = mempty
        , oo_render = img
        , oo_state =
            os & #os_pos .~ pos'
               & #os_collision .~ coerce (Just ore)
        }
      , (pos', vel'')
      )


touchingGround :: (V2 WorldPos -> Bool) -> OriginRect WorldPos -> V2 WorldPos -> Bool
touchingGround toHit ore pos = let touchDist = V2 0 1
  in or $ fmap toHit (cornersX ore Positive (pos + touchDist))

updateVel :: Bool -> V2 Double -> V2 Double -> V2 Double
updateVel onGround vel dvel = let
  grav = V2 0 10
  dir = signum $ vel
  dir' = signum $ dvel
  maxXSpeed = 100
  walkVel = maxXSpeed *^ dir'
  freeVel = vel + clampJump onGround dvel + grav
  freeXV = freeVel ^. _x :: Double
  maxXV = maxXSpeed * (dir ^. _x) :: Double
  cappedXV = case compare (abs freeXV) maxXSpeed of {LT -> freeXV; _ -> maxXV}
  cappedFreeVel = freeVel & _x .~ cappedXV
  groundVel = V2 1 0 * walkVel + V2 0 1 * freeVel
  in if onGround then groundVel else cappedFreeVel

clampJump :: Bool -> V2 Double -> V2 Double
clampJump True = id
clampJump False = _y %~ max 0

