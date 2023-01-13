module Collision where

import Control.Lens (Lens')
import Geometry
import SDL
import Engine.Types
import Utils

data DeltaDir = Negative | Zero | Positive
  deriving (Eq, Ord, Show, Enum, Bounded)

hitTile :: (V2 Tile -> Bool) -> V2 WorldPos -> Bool
hitTile f = f . posToTile


makeLine :: (Floating a, RealFrac a) => V2 a -> V2 a -> [V2 a]
makeLine a b = do
  let dist = distance a b
      n = round @_ @Int dist
  case dist <= tileSize of
    True -> [a, b]
    False -> do
      ix <- [0 .. n]
      pure $ a + (b - a) / fromIntegral n * fromIntegral ix


cornersX :: (RealFrac a, Floating a) => OriginRect a -> DeltaDir -> V2 a -> [V2 a]
cornersX ore Negative pos = makeLine (orTopLeft pos ore) (orTopRight pos ore)
cornersX _ Zero pos = pure pos
cornersX ore Positive pos = makeLine (orBotLeft pos ore) (orBotRight pos ore)


cornersY :: (RealFrac a, Floating a) => OriginRect a -> DeltaDir -> V2 a -> [V2 a]
cornersY ore Negative pos = makeLine (orTopLeft pos ore) (orBotLeft pos ore)
cornersY _ Zero pos = pure pos
cornersY ore Positive pos = makeLine (orTopRight pos ore) (orBotRight pos ore)


move
    :: (CollisionPurpose -> V2 WorldPos -> Bool)
    -> OriginRect Double
    -> V2 WorldPos
    -> V2 Double
    -> Maybe (V2 WorldPos)
move f sz pos dpos = do
  let (V2 xd yd) = fmap deltaDir dpos
      subdivs :: Int
      subdivs = min 10 $ max 1 $ ceiling (norm dpos / tileSize)

      dpos' :: V2 Double
      dpos' = dpos / fromIntegral subdivs

  sufficientlyDifferent pos
    $ head
    $ drop subdivs
    $ iterate ( moveX (f CollisionWall) sz xd dpos'
              . moveY (f (bool CollisionCeiling CollisionGround $ yd == Positive)) sz yd dpos'
               )
    $ pos


sufficientlyDifferent :: V2 WorldPos -> V2 WorldPos -> Maybe (V2 WorldPos)
sufficientlyDifferent pos pos' =
  if qd pos pos' <= epsilon * 5
     then Nothing
     else Just pos'


deltaDir :: RealFrac a => a -> DeltaDir
deltaDir n =
  case round @_ @Int $ signum n of
    -1 -> Negative
    0 ->  Zero
    1 ->  Positive
    _ ->  error "impossible"

epsilon :: Fractional a => a
epsilon = 0.001



moveXY
    :: (OriginRect WorldPos -> DeltaDir -> V2 WorldPos -> [V2 WorldPos])
    -> (OriginRect WorldPos -> V2 WorldPos)
    -> (OriginRect WorldPos -> V2 WorldPos)
    -> Lens' (V2 WorldPos) WorldPos
    -> Lens' (V2 WorldPos) WorldPos
    -> (V2 WorldPos -> Bool)
    -> OriginRect Double
    -> DeltaDir
    -> V2 Double
    -> V2 WorldPos
    -> V2 WorldPos
moveXY cs ld rd coord to_zero f (coerce -> sz) xdir (coerce -> dpos) pos0 =
  let pos = pos0 + (dpos & to_zero .~ 0) in

  case any f $ cs sz xdir pos of
    False -> pos
    True ->
      case xdir of
        Negative -> pos & coord .~ coerce ((tileToPos (posToTile (pos - ld sz) + 1) + ld sz + epsilon) ^. coord)
        Zero -> pos -- already in the wall
        Positive -> pos & coord .~ coerce ((tileToPos (posToTile $ pos + rd sz) - rd sz - epsilon) ^. coord)


moveX :: (V2 WorldPos -> Bool) -> OriginRect Double -> DeltaDir -> V2 Double -> V2 WorldPos -> V2 WorldPos
moveX = moveXY cornersY orLeftDist orRightDist _x _y


moveY :: (V2 WorldPos -> Bool) -> OriginRect Double -> DeltaDir -> V2 Double -> V2 WorldPos -> V2 WorldPos
moveY = moveXY cornersX orTopDist orBotDist _y _x

