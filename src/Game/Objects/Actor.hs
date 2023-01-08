module Game.Objects.Actor where

import Types
import FRP
import Collision
import Utils
import Data.Bool (bool)


actor
    :: OriginRect WorldPos
    -> SF FrameInfo (V2 Double)
    -> SF (ObjectInput, V2 WorldPos) Renderable
    -> V2 WorldPos
    -> SF (ObjectInput) ObjectOutput
actor ore input render pos0 = loopPre (pos0, 0) $
  proc ((oi@(ObjectInput _ fi)), (pos, vel)) -> do
    let dt = fi_dt fi
    let lev = gs_currentLevel $ fi_global fi

    vel'0 <- input -< fi

    let onGround = touchingGround (l_hitmap lev Layer1 . posToTile) ore pos
    let vel' = updateVel onGround vel vel'0 
    let dpos = dt *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move (l_hitmap lev Layer1 . posToTile) (coerce ore) pos $ dpos

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
        , oo_pos = pos'
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
  freeVel = vel + dvel + grav
  freeXV = freeVel ^. _x :: Double
  maxXV = maxXSpeed * (dir ^. _x) :: Double
  cappedXV = case compare (abs freeXV) maxXSpeed of {LT -> freeXV; _ -> maxXV}
  cappedFreeVel = freeVel & _x .~ cappedXV
  groundVel = V2 1 0 * walkVel + V2 0 1 * freeVel
  in if onGround then groundVel else cappedFreeVel
