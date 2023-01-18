module Game.Objects.TeleportBall where

import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Engine.Collision (move)
import           Engine.Globals (global_textures)
import           Game.Common
import           Game.Objects.Particle (teleportDie)

teleportBall
    :: ObjectId
    -> OriginRect Double
    -> V2 WorldPos
    -> V2 WorldPos
    -> V2 Double
    -> Object
teleportBall owner owner_ore pos0 offset vel0@(V2 vx _) = dieOnPlayerDeath $
  dSwitch (charge owner pos0 offset $ round $ signum vx) $ \(power) ->
    loopPre (vel0 ^* power) $ proc (oi, vel) -> do
      die <- after 1 () -< ()

      start <- nowish () -< ()
      let pos = os_pos $ oi_state oi
          dt = fi_dt $ oi_frameInfo oi

      let vel' = vel
      -- let pos' = pos + coerce (vel ^* dt)
      let ore = mkCenterdOriginRect 8
      let gs = fi_global $ oi_frameInfo oi

          mpos' = move (getCollisionMap gs) (coerce ore) pos (vel ^* dt)
          pos' = fromMaybe pos mpos'

          vel'' = maybe vel' (coerce . subtract pos) (coerce mpos') ^* (1 / dt)
                + grav ^* dt

      let end = mergeEvents
            [ die
            , bool (Event ()) noEvent $ isJust mpos'
            ]

      -- find the last place we could place the character
      youpos'ev <- arr maybeToEvent -<
          move (getCollisionMap gs) (coerce owner_ore) pos (vel ^* dt)
      youpos' <- hold pos0 -< youpos'ev

      t <- time -< ()

      returnA -< (, vel'') $ ObjectOutput
        { oo_events = mempty
            { oe_die = end
            , oe_send_message = [(owner, TeleportTo youpos')] <$ end
            , oe_focus = () <$ start
            , oe_play_sound = [WarpSound] <$ end
            }
        , oo_render = drawGameTextureOriginRect TeleTexture ore pos' (t * 360) (pure False)
        , oo_state = (noObjectState pos')
          { os_camera_offset = vel' ^* (dt * 10)
            }
        }

charge :: ObjectId -> V2 WorldPos -> V2 WorldPos ->  Int -> SF ObjectInput (ObjectOutput, Event (Double))
charge owner pos0 offset dir = proc oi -> do
  (progress, done)
    <- charging 0.5 (arr $ not . c_z . fi_controls . oi_frameInfo)
    -< oi

  let pos = (maybe pos0 os_pos $ M.lookup owner $ oi_everyone oi) + offset

  returnA -< (, (done)) $ ObjectOutput
    { oo_events = mempty
    , oo_render =
        drawTextureOriginRect
          (setCenterOrigin (global_textures ChargeTexture) & #wt_origin . _x .~ 0)
          (coerce $ OriginRect (V2 (progress * 30) 10) 0)
          (pos + bool 0 (V2 (-5) 8) (dir < 0))
          (-45 - bool 0 90 (dir < 0))
          (pure False)
    , oo_state = noObjectState pos
    }


-- TODO(sandy): janky; does not do what it says on the tin
dieOnPlayerDeath :: Object -> Object
dieOnPlayerDeath obj = proc oi -> do
  oo <- obj -< oi
  let player_death = listenInbox (preview #_PlayerDeath . snd) $ oi_events oi
  let pos = os_pos $ oo_state oo
  returnA -< oo
    & #oo_events . #oe_die <>~ player_death
    & #oo_events . #oe_spawn <>~ (teleportDie pos <$ player_death)


grav :: V2 Double
grav = V2 0 210

