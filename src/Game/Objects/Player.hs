module Game.Objects.Player where

import Types
import FRP
import Game.Objects.Actor (actor)
import Utils
import Drawing
import Control.Lens ((*~))

mkCenterdOriginRect :: Fractional a => V2 a -> OriginRect a
mkCenterdOriginRect sz = OriginRect sz (sz / 2)

player :: V2 WorldPos -> Object
player pos0
  = Object noObjectMeta
  $ actor ore playerPhysVelocity (drawPlayer ore) pos0
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
      [ drawOriginRect (V4 255 255 0 64) sz pos
      , drawFilledRect (V4 255 0 0 255)
          $ flip Rectangle 1
          $ P
          $ pos
      ]
  , arr (\(_, pos) -> (Run, pos)) >>> mkAnim MainCharacter
  ]
