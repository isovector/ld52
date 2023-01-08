module Game.Objects.Actor where

import Types
import FRP
import Collision
import Utils
import Data.Bool (bool)


actor
    :: V2 Double
    -> SF FrameInfo (V2 Double)
    -> SF (ObjectInput, V2 WorldPos) Renderable
    -> V2 WorldPos
    -> SF (Level, ObjectInput) ObjectOutput
actor sz input render pos0 = loopPre (pos0, 0) $
  proc ((lev, oi@(ObjectInput _ fi)), (pos, vel)) -> do
    let dt = fi_dt fi

    vel'0 <- input -< fi

    let (lbc, rbc) = cornersX sz 1 pos
    let onGround = or $ fmap (l_hitmap lev Layer1 . posToTile . (+ V2 0 1)) [lbc, rbc]

    let grav = V2 0 10
    let dir' = signum $ vel'0
    let maxXSpeed = 100
    let walkVel = maxXSpeed *^ dir'
    let uncappedVel = if onGround then V2 1 0 * walkVel + V2 0 1 * (vel + vel'0 + grav) else vel + vel'0 + grav
    let vel' = (case compare (abs (uncappedVel ^. _x)) maxXSpeed of
           LT -> uncappedVel
           _ -> uncappedVel & _x .~ ((dir' ^. _x) * maxXSpeed))

    let dpos = dt *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move (l_hitmap lev Layer1 . posToTile) sz pos $ dpos

    let vel''
          = (\want have res -> bool 0 res $ want == have)
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
