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
    -> SF (Level, ObjectInput) ObjectOutput
actor ore input render pos0 = loopPre (pos0, 0) $
  proc ((lev, oi@(ObjectInput _ fi)), (pos, vel)) -> do
    let dt = fi_dt fi

    vel'0 <- input -< fi

    let grav = V2 0 10

    let dir = signum $ vel
    let dir' = signum $ vel'0
    let maxXSpeed = 100
    let walkVel = maxXSpeed *^ dir'
    let onGround = touchingGround (l_hitmap lev Layer1 . posToTile) ore pos
    
    let freeVel = vel + vel'0 + grav
    let freeXV = freeVel ^. _x :: Double
    let maxXV = maxXSpeed * (dir ^. _x) :: Double
    let cappedXV = (case compare (abs freeXV) maxXSpeed of
           LT -> freeXV
           _ -> maxXV)  :: Double
    let cappedFreeVel = freeVel & _x .~ cappedXV
    let groundVel = V2 1 0 * walkVel + V2 0 1 * freeVel
    let vel' = if onGround then groundVel else cappedFreeVel

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