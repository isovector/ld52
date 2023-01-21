{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import           Engine.Collision
import           Engine.Drawing
import           FRP.Yampa ((*^))
import           Game.Common
import           Game.Objects.Particle (gore, teleportIn)
import           Game.Objects.TeleportBall (teleportBall)
import qualified SDL.Vect as SDL
import Control.Monad (void, join)

player :: V2 WorldPos -> Object
player pos0 = loopPre 0 $ proc (oi, vel) -> do
  -- TODO(sandy): this is a bad pattern; object constructor should take an
  -- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start

  let am_teleporting
        = listenInbox (preview #_TeleportTo . snd)
        $ oi_events oi

      do_teleport :: V2 WorldPos -> V2 WorldPos
      do_teleport = event id const am_teleporting

  let dir0 = os_facing $ oi_state oi

  reset <- edge -< c_reset $ fi_controls $ oi_frameInfo oi
  let cp_hit = listenInbox (\(from, m) -> (from, ) <$> preview #_SetCheckpoint m) $ oi_events oi
  cp_pos <- hold pos0 -< fmap snd cp_hit

  let dying = merge reset
            $ listenInbox (preview #_Die . snd)
            $ oi_events oi


      at_door :: Event (V2 WorldPos)
      at_door = listenInbox (preview #_TeleportOpportunity . snd)
              $ oi_events oi

      doorout :: Maybe (V2 WorldPos)
      doorout = eventToMaybe at_door

      tramp :: Maybe Double
      tramp = eventToMaybe
              $ listenInbox (preview #_OnTrampoline . snd)
              $ oi_events oi

      powerups :: S.Set PowerupType
      powerups = gs_inventory $ gameState oi

  RateLimited (isNothing -> alive) respawn death_evs <- dieAndRespawnHandler -< (pos, dying)


  let can_double_jump = S.member PowerupDoubleJump powerups
  vel''0 <- playerPhysVelocity -< oi_frameInfo oi
  let vel'0 = vel''0 & _y %~ maybe id const tramp


  let collision = getCollisionMap $ globalState oi

  let dt = deltaTime oi

  let onGround = touchingGround (collision CollisionCheckGround) ore pos
  let vel2' = updateVel (can_double_jump || onGround) dt vel vel'0

  wants_totsugeki <- edge -< c_c (controls oi) && alive && S.member PowerupTotsugeki powerups
  totsugeki <- totsugekiHandler -< (wants_totsugeki, dir0, pos)

  let vel' = fromMaybe vel2' totsugeki

  let dpos = vel' ^* dt

  let desiredPos = pos + coerce dpos
  let pos' = fromMaybe pos $ move collision (coerce ore) pos $ dpos

  throw_ball <- edge -< c_z $ fi_controls $ oi_frameInfo oi
  throw_evs <-
    throwBallHandler (- (sz & _x .~ 0)) ore
      -< ( oi_self oi
         , pos
         , dir0
         , vel'
         , whenE (S.member PowerupWarpBall powerups) $ whenE alive throw_ball
         )

  let vel''
        = (\want have res -> bool 0 res $ abs(want - have) <= epsilon )
            <$> desiredPos
            <*> pos'
            <*> vel'

  press_up <- edge -< view _y $ fmap (== -1) $ c_dir $ fi_controls $ oi_frameInfo oi

  let won = press_up >> at_door

  let pos'' = bool (const pos) id alive
          $ bool id (const cp_pos) (isEvent respawn)
          $ bool id (maybe id const doorout) (event False (const True) press_up)
          $ do_teleport
          $ pos'

  edir <- edgeBy diffDir 0 -< pos''
  edir' <- onChange -< edir
  dir <- hold True -< whenE (alive && not (isEvent respawn)) $ join edir'
  dir_change <- onChange -< dir

  t <- localTime -< ()
  last_edir_time <- hold 0 -< t <$ dir_change
  let offset_size = min 1 $ (t - last_edir_time) / 2

  let V2 _ updowndir = c_dir $ fi_controls $ oi_frameInfo oi

  drawn <- drawPlayer -< (dir, pos'', isJust totsugeki)

  returnA -< (, bool 0 vel'' (alive && not (isEvent am_teleporting))) $
    ObjectOutput
        { oo_events = (mconcat [death_evs, throw_evs] <>) $
            mempty
              & #oe_focus .~ mconcat
                  [ () <$ am_teleporting
                  , start
                  ]
              & #oe_broadcast_message .~ fmap (pure . CurrentCheckpoint . fst) cp_hit
              & #oe_game_message .~ ([GameWon] <$ won)
              & #oe_spawn .~ (teleportIn 0.50 (-60) pos <$ am_teleporting)
        , oo_state =
            oi_state oi
              & #os_pos .~ pos''
              & #os_camera_offset .~ V2 (bool negate id dir (120 * offset_size) * max 0.3 (1 - abs (fromIntegral updowndir)))
                                        (case updowndir of
                                           -1 -> -100
                                           0 -> -50
                                           1 -> 75
                                           _ -> error "impossible: player cam"
                                        )
              & #os_collision .~ bool Nothing (Just ore) alive
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ dir
        , oo_render = ifA alive drawn
        }
  where
    ore = OriginRect sz $ sz & _x *~ 0.5

    sz :: Num a => V2 a
    sz = V2 8 16


touchingGround :: (V2 WorldPos -> Bool) -> OriginRect Double -> V2 WorldPos -> Bool
touchingGround toHit ore pos =
    or
      $ fmap toHit
      $ cornersX (coerce ore) Positive
      $ pos + touchDist
  where
  touchDist = V2 0 1


updateVelAir :: Time -> V2 Double -> V2 Double -> V2 Double
updateVelAir dt vel dvel =
    freeVel & _x %~ clampAbs maxXSpeed
  where
    maxXSpeed = 110
    freeVel = vel + ((dvel & _y %~ max 0) + gravity) ^* dt

updateVelGround :: Time -> V2 Double -> V2 Double -> V2 Double
updateVelGround dt vel dvel@(V2 dvx _) =
    V2 (maxXSpeed * signum dvx) air_y
  where
    maxXSpeed = 110
    (V2 _ air_y) = vel + dvel + gravity ^* dt

gravity :: Num a => V2 a
gravity = V2 0 625

updateVel :: Bool -> Time -> V2 Double -> V2 Double -> V2 Double
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



respawnTime :: Time
respawnTime = 1


throwBallHandler
    :: V2 WorldPos
    -> OriginRect Double
    -> SF (ObjectId, V2 WorldPos, Bool, V2 Double, Event a) ObjectEvents
throwBallHandler offset ore =
  proc (me, pos, dir, vel, throw) ->
    fmap rl_data $ rateLimit 1.5 (
      proc (ev, (me, pos, dir, vel)) -> do
        returnA -< mempty
          & #oe_spawn .~
              ([teleportBall me ore pos offset (vel + V2 (bool negate id dir 200) (-200))] <$ ev)
      ) -< (throw, (me, pos, dir, vel))


dieAndRespawnHandler :: SF (V2 WorldPos, Event a) (RateLimited ObjectEvents)
dieAndRespawnHandler = proc (pos, on_die) -> do
  rateLimit respawnTime
     (arr $ \(ev, pos) ->
        mempty
          & #oe_spawn .~ (gore pos <$ ev)
          & #oe_play_sound .~ ([DieSound] <$ ev)
          & #oe_broadcast_message .~ ([PlayerDeath] <$ ev)
          & #oe_game_message .~ ([AddPlayerDeath] <$ ev)
          & #oe_focus .~ void ev
     ) -< (on_die, pos)


totsugekiHandler :: SF (Event a, Bool, V2 WorldPos) (Maybe (V2 Double))
totsugekiHandler = proc (ev, dir, pos) -> do
  RateLimited cooldown _ _ <- rateLimit totsugeki_time identity -< (ev, pos)
  let active = maybe False (>= (totsugeki_time - 0.5)) cooldown

  returnA -< bool Nothing (Just $ V2 (bool negate id dir 400) 0) active
  where
    totsugeki_time = 1


playerPhysVelocity :: SF FrameInfo (V2 Double)
playerPhysVelocity = proc fi -> do
  let jumpVel = V2 0 (-220)
  let stepSpeed = 11 * 60
  jumpEv <- edge -< c_space (fi_controls fi)
  let jump = event 0 (const jumpVel) jumpEv
  let vx = V2 stepSpeed 0 * (realToFrac <$> c_dir (fi_controls fi))
  let vy = jump
  let vel' = vx + vy
  returnA -< vel'


drawPlayer :: SF (Bool, V2 WorldPos, Bool) Renderable
drawPlayer =
  proc (dir, pos, is_totsugeku) -> do
    -- We can fully animate the player as a function of the position!
    V2 vx vy <- derivative -< pos
    r <- mkAnim
        -<  ( DrawSpriteDetails
                (bool (Idle MainCharacter) (Run MainCharacter) $ abs vx >= epsilon && abs vy < epsilon && not is_totsugeku)
                0
                (V2 (not dir) False)
            , pos
            )
    returnA -< mconcat
      [ ifA is_totsugeku
          $ drawGameTextureOriginRect
              ChickenTexture
              (mkCenterdOriginRect 24 & #orect_offset . _x -~ bool (-10) 10 dir) pos 0
          $ V2 dir False
      , r
      ]



instance (Floating a, Eq a) => VectorSpace (V2 a) a where
  zeroVector = 0
  (*^) = (Game.Common.*^)
  (^+^) = (+)
  dot = SDL.dot


diffDir :: (Ord a, Floating a) => V2 a -> V2 a -> Maybe Bool
diffDir (V2 old _) (V2 new _) =
  case abs (old - new) <= epsilon of
    True -> Nothing
    False -> Just $ new > old

