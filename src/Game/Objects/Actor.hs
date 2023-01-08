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

    let grav = V2 0 10
    let vel' = vel + vel'0 + grav

    let dpos = dt *^ vel'
    let desiredPos = pos + coerce dpos
    let pos' = move (l_hitmap lev Layer1 . posToTile) sz pos $ dpos

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

