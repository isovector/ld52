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
import           Game.Objects.Actor (actor)
import           Game.Objects.TeleportBall (teleportBall)
import qualified SDL.Vect as SDL
import           Types
import           Utils
import Game.Common (listenInbox)


player :: V2 WorldPos -> Object
player pos0
  = ( loopPre False $ proc (oi, can_double_jump0) -> do
        let now_jump
              = event False ( any $ any (== PowerupDoubleJump)
                                  . mapMaybe (preview #_IsPowerup)
                                  . toList
                                  . os_tags
                                  . snd
                            ) $ oie_hit $ oi_events oi
        let do_teleport :: V2 WorldPos -> V2 WorldPos
            do_teleport
              = event id ( appEndo
                         . foldMap ( foldMap (Endo . const)
                                   . preview #_TeleportTo
                                   )
                         )
              $ oie_receive
              $ oi_events oi

        cp_pos <- hold pos0 -< listenInbox (preview #_SetCheckpoint) $ oi_events oi
        let dying :: Bool
            dying = event False (const True)
                  $ listenInbox (preview #_Die)
                  $ oi_events oi

        let me = oi_self oi
        action <- edge -< c_z $ fi_controls $ oi_frameInfo oi

        let can_double_jump = can_double_jump0 || now_jump
        res <- actor ore playerPhysVelocity (drawPlayer ore) pos0 -< (can_double_jump, oi)
        let pos = bool id (const cp_pos) dying
                $ do_teleport
                $ res ^. #oo_state . #os_pos

        edir <- edgeBy diffDir 0 -< pos
        dir <- hold True -< edir

        returnA -<
          ( res & #oo_state . #os_tags
                    %~ bool id (S.insert $ HasPowerup PowerupDoubleJump) now_jump
                & #oo_state . #os_pos .~ pos
                & #oo_state . #os_tags %~ S.insert IsPlayer
                & #oo_events . #oe_spawn <>~ ([teleportBall me pos $ V2 (bool negate id dir 200) (-100)] <$ action)
          , can_double_jump
          )
    )
  -- >>> actor ore playerPhysVelocity (drawPlayer ore) pos0
  >>> focusOn
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


drawPlayer :: OriginRect WorldPos -> SF (ObjectInput, V2 WorldPos) Renderable
drawPlayer sz = arr mconcat <<< fork
  [ arr $ \(_, pos) -> mconcat
      [ drawOriginRect (V4 255 255 0 16) sz pos
      , drawFilledRect (V4 255 0 0 255)
          $ flip Rectangle 1
          $ P
          $ pos
      ]
  , proc (_, pos) -> do
      -- We can fully animate the player as a function of the position!
      edir <- edgeBy diffDir 0 -< pos
      dir <- hold True -< edir
      vel <- derivative -< pos
      mkAnim MainCharacter
        -<  ( DrawSpriteDetails
                (bool Idle Run $ norm vel >= epsilon)
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

