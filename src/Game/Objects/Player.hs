{-# OPTIONS_GHC -Wno-orphans #-}
module Game.Objects.Player where

import           Collision (epsilon)
import           Control.Lens ((*~), preview, (<>~))
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import qualified Data.Set as S
import           Drawing
import           FRP
import           FRP.Yampa ((*^))
import           Game.Common (listenInbox)
import           Game.Objects.Actor (actor)
import           Game.Objects.TeleportBall (teleportBall)
import qualified SDL.Vect as SDL
import           Types
import           Utils
import Game.Objects.Particle (gore)

player :: V2 WorldPos -> Object
player pos0 = proc oi -> do
  -- TODO(sandy): this is a bad pattern; object constructor should take an
  -- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start


  let new_powerups
        = flip fmap (oie_hit (oi_events oi))
        $ foldMap
        $ mapMaybe (preview #_IsPowerup) . toList . os_tags . snd

  let am_teleporting
        = fmap (foldMap (foldMap (Endo . const) . preview #_TeleportTo . snd))
        . oie_receive
        $ oi_events oi

      do_teleport :: V2 WorldPos -> V2 WorldPos
      do_teleport = event id appEndo am_teleporting

  reset <- edge -< c_reset $ fi_controls $ oi_frameInfo oi
  let cp_hit = listenInbox (\(from, m) -> (from, ) <$> preview #_SetCheckpoint m) $ oi_events oi
  cp_pos <- hold pos0 -< fmap snd cp_hit

  let dying :: Bool
      dying = event False (const True)
            $ merge reset
            $ listenInbox (preview #_Die . snd)
            $ oi_events oi

      doorout :: Maybe (V2 WorldPos)
      doorout = eventToMaybe
              $ listenInbox (preview #_TeleportOpportunity . snd)
              $ oi_events oi

      tramp :: Maybe Double
      tramp = eventToMaybe
              $ listenInbox (preview #_OnTrampoline . snd)
              $ oi_events oi

      powerups :: S.Set PowerupType
      powerups
        = flip foldMap (os_tags $ oi_state oi)
        $ foldMap S.singleton
        . preview #_HasPowerup

  let me = oi_self oi
  action <- edge -< c_z $ fi_controls $ oi_frameInfo oi
  let throw_ball = bool noEvent action $ S.member PowerupWarpBall powerups

  let can_double_jump = S.member PowerupDoubleJump powerups
      dt = fi_dt $ oi_frameInfo oi
  vel'0 <- playerPhysVelocity -< oi_frameInfo oi
  pos' <- actor ore -<
    ( can_double_jump
    , dt
    , vel'0 & _y %~ maybe id const tramp
    , pos
    , fi_global $ oi_frameInfo oi
    )

  let (V2 _ press_up) = fmap (== -1) $ c_dir $ fi_controls $ oi_frameInfo oi
  let pos'' = bool id (const cp_pos) dying
          $ bool id (maybe id const doorout) press_up
          $ do_teleport
          $ pos'

  edir <- edgeBy diffDir 0 -< pos
  dir <- hold True -< edir

  let V2 _ updowndir = c_dir $ fi_controls $ oi_frameInfo oi

  drawn <- drawPlayer ore -< pos''

  returnA -<
    ObjectOutput
        { oo_events =
            mempty
              & #oe_spawn <>~
                  ([teleportBall
                        me
                        (coerce ore) (pos - (sz & _x .~ 0))
                      $ V2 (bool negate id dir 200) (-200)] <$ throw_ball
                  ) <> (bool noEvent (Event $ gore pos) dying)
              & #oe_focus .~ mconcat
                  [ () <$ am_teleporting
                  , start
                  ]
              & #oe_broadcast_message <>~ fmap (pure . CurrentCheckpoint . fst) cp_hit
        , oo_state =
            oi_state oi
              & #os_pos .~ pos''
              & #os_camera_offset .~ V2 (bool negate id dir 80 * max 0.3 (1 - abs (fromIntegral updowndir)))
                                        (case updowndir of
                                           -1 -> -100
                                           0 -> -50
                                           1 -> 75
                                           _ -> error "impossible: player cam"
                                        )
              & #os_collision .~ Just (coerce ore)
              & #os_tags %~ event id (flip (foldr $ S.insert . HasPowerup)) new_powerups
              & #os_tags %~ S.insert IsPlayer
        , oo_render = drawn
        }
  where
    ore = OriginRect sz $ sz & _x *~ 0.5

    sz :: Num a => V2 a
    sz = V2 8 16


playerPhysVelocity :: SF FrameInfo (V2 Double)
playerPhysVelocity = proc fi -> do
  let jumpVel = V2 0 (-200)
  let stepSpeed = 10
  jumpEv <- edge -< c_space (fi_controls fi) -- TODO: Only jump when on the ground
  let jump = event 0 (const jumpVel) jumpEv
  let vx = V2 stepSpeed 0 * (realToFrac <$> c_dir (fi_controls fi))
  let vy = jump
  let vel' = vx + vy
  returnA -< vel'


drawPlayer :: OriginRect WorldPos -> SF (V2 WorldPos) Renderable
drawPlayer sz = arr mconcat <<< fork
  -- [ arr $ \pos -> mconcat
  --     [ drawOriginRect (V4 255 255 0 16) sz pos
  --     , drawFilledRect (V4 255 0 0 255)
  --         $ flip Rectangle 1
  --         $ P
  --         $ pos
  --     ]
  [ proc pos -> do
      -- We can fully animate the player as a function of the position!
      edir <- edgeBy diffDir 0 -< pos
      dir <- hold True -< edir
      V2 vx vy <- derivative -< pos
      mkAnim MainCharacter
        -<  ( DrawSpriteDetails
                (bool Idle Run $ abs vx >= epsilon && abs vy < epsilon)
                0
                (V2 (not dir) False)
            , pos
            )
  ]


instance VectorSpace (V2 WorldPos) WorldPos where
  zeroVector = 0
  (*^) = (Types.*^)
  (^+^) = (+)
  dot = SDL.dot


diffDir :: ( Ord a, Floating a) =>V2 a -> V2 a -> Maybe Bool
diffDir (V2 old _) (V2 new _) =
  case abs (old - new) <= epsilon of
    True -> Nothing
    False -> Just $ new > old

