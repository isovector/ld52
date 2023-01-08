module Game.Objects.Player where

import Types
import FRP
import Game.Objects.Actor (actor)
import Utils
import Drawing

player :: Resources -> Object
player rs
  = Object noObjectMeta
  $ arr (head $ toList $ w_levels $ r_worlds rs TestWorld ,)
    >>> actor 32 playerPhysVelocity (drawPlayer rs 32) (V2 0 30)
    >>> focusOn

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

drawPlayer :: Resources -> V2 Double -> SF (ObjectInput, V2 WorldPos) Renderable
drawPlayer rs sz = arr $ \(_, pos) -> mconcat
  [ drawFilledRect (V4 255 255 0 64)
      $ flip Rectangle (coerce sz)
      $ P
      $ pos - coerce sz / 2
  , drawFilledRect (V4 255 0 0 255)
      $ flip Rectangle 1
      $ P
      $ pos
  -- , drawSprite
  --     (setGroundOrigin $ r_textures rs MainCharacter)
  --     (pos - coerce sz / 2)
  --     0
  --     (V2 False False)
  ]
