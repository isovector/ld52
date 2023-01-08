module Collision where

import SDL
import Types
import Utils

data DeltaDir = Negative | Zero | Positive

hitTile :: (V2 Tile -> Bool) -> V2 WorldPos -> Bool
hitTile f = f . posToTile

testOR :: OriginRect Double
testOR = OriginRect 16 8

orTopLeft :: Num a => V2 a -> OriginRect a -> V2 a
orTopLeft pos ore = pos - orect_offset ore

orBotLeft :: Num a => V2 a -> OriginRect a -> V2 a
orBotLeft pos ore = orTopLeft pos ore + (orect_size ore & _x .~ 0)

orTopRight :: Num a => V2 a -> OriginRect a -> V2 a
orTopRight pos ore = orTopLeft pos ore + (orect_size ore & _y .~ 0)

orBotRight :: Num a => V2 a -> OriginRect a -> V2 a
orBotRight pos ore = orTopLeft pos ore + orect_size ore

orTopDist :: Num a => OriginRect a -> V2 a
orTopDist ore = orect_offset ore & _x .~ 0

orBotDist :: Num a => OriginRect a -> V2 a
orBotDist ore = (ore ^. #orect_size - ore ^. #orect_offset) & _x .~ 0

orLeftDist :: Num a => OriginRect a -> V2 a
orLeftDist ore = orect_offset ore & _y .~ 0

orRightDist :: Num a => OriginRect a -> V2 a
orRightDist ore = (ore ^. #orect_size - ore ^. #orect_offset) & _y .~ 0

makeLine :: V2 a -> V2 a -> [V2 a]
makeLine a b = [a, b]

cornersX :: Num a => OriginRect a -> DeltaDir -> V2 a -> [V2 a]
cornersX ore Negative pos = makeLine (orTopLeft pos ore) (orTopRight pos ore)
cornersX _ Zero pos = pure pos
cornersX ore Positive pos = makeLine (orBotLeft pos ore) (orBotRight pos ore)

cornersY :: Num a => OriginRect a -> DeltaDir -> V2 a -> [V2 a]
cornersY ore Negative pos = makeLine (orTopLeft pos ore) (orBotLeft pos ore)
cornersY _ Zero pos = pure pos
cornersY ore Positive pos = makeLine (orTopRight pos ore) (orBotRight pos ore)


move :: (V2 WorldPos -> Bool) -> OriginRect Double -> V2 WorldPos -> V2 Double -> V2 WorldPos
move f sz pos (dpos) = do
  let (V2 xd yd) = fmap deltaDir dpos
      subdivs :: Int
      subdivs = ceiling $ norm dpos
  head
    $ drop subdivs
    $ iterate (moveX f sz xd . moveY f sz yd . (+ coerce dpos / fromIntegral subdivs))
    $ pos

deltaDir :: RealFrac a => a -> DeltaDir
deltaDir n =
  case round @_ @Int $ signum n of
    -1 -> Negative
    0 ->  Zero
    1 ->  Positive
    _ ->  error "impossible"

epsilon :: Fractional a => a
epsilon = 0.001


moveX :: (V2 WorldPos -> Bool) -> OriginRect Double -> DeltaDir -> V2 WorldPos -> V2 WorldPos
moveX f (coerce -> sz) xdir pos =
  case any f $ cornersY sz xdir pos of
    False -> pos
    True ->
      case xdir of
        Negative -> pos & _x .~ coerce ((tileToPos (posToTile (pos - orLeftDist sz) + 1) + orLeftDist sz + epsilon) ^. _x)
        Zero -> pos -- already in the wall
        Positive -> pos & _x .~ coerce ((tileToPos (posToTile $ pos + orRightDist sz) - orRightDist sz - epsilon) ^. _x)


moveY :: (V2 WorldPos -> Bool) -> OriginRect Double -> DeltaDir -> V2 WorldPos -> V2 WorldPos
moveY f (coerce -> sz) ydir pos =
  case any f $ cornersX sz ydir pos of
    False -> pos
    True ->
      case ydir of
        Negative -> pos & _y .~ coerce ((tileToPos (posToTile (pos - orTopDist sz) + 1) + orTopDist sz + epsilon) ^. _y)
        Zero -> pos -- already in the wall
        Positive -> pos & _y .~ coerce ((tileToPos (posToTile $ pos + orBotDist sz) - orBotDist sz - epsilon) ^. _y)

