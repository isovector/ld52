module Game.Objects.Actor where

import Collision
import Game.Common


actor
    :: OriginRect WorldPos
    -> SF (Bool, Double, V2 Double, V2 WorldPos, GlobalState) (V2 WorldPos)
actor ore = loopPre 0 $
  proc ((can_double, _dt, vel'0, pos, gs), vel) -> do
    let collision = getCollisionMap gs

    let onGround = touchingGround (collision CollisionCheckGround) ore pos
    let vel' = updateVel (can_double || onGround) vel vel'0
    let dpos = 0.016 *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = fromMaybe pos $ move collision (coerce ore) pos $ dpos

    let vel''
          = (\want have res -> bool 0 res $ abs(want - have) <= epsilon )
              <$> desiredPos
              <*> pos'
              <*> vel'

    returnA -< (pos', vel'')


touchingGround :: (V2 WorldPos -> Bool) -> OriginRect WorldPos -> V2 WorldPos -> Bool
touchingGround toHit ore pos =
    or
      $ fmap toHit
      $ cornersX ore Positive
      $ pos + touchDist
  where
  touchDist = V2 0 1


updateVelAir :: V2 Double -> V2 Double -> V2 Double
updateVelAir vel dvel =
    freeVel & _x %~ clampAbs maxXSpeed
  where
    grav = V2 0 10
    maxXSpeed = 110
    freeVel = vel + (dvel & _y %~ max 0) + grav

updateVelGround :: V2 Double -> V2 Double -> V2 Double
updateVelGround vel dvel@(V2 dvx _) =
    V2 (maxXSpeed * signum dvx) air_y
  where
    maxXSpeed = 110
    grav = V2 0 10
    (V2 _ air_y) = vel + dvel + grav


updateVel :: Bool -> V2 Double -> V2 Double -> V2 Double
updateVel True = updateVelGround
updateVel False = updateVelAir

clampAbs :: (Num a, Ord a) => a -> a -> a
clampAbs maxv val =
  if abs val <= maxv
     then val
     else maxv * signum val

clampJump :: Bool -> V2 Double -> V2 Double
clampJump True = id
clampJump False = _y %~ max 0

