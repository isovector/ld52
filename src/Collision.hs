module Collision where

import SDL
import Types
import Utils


hitTile :: (V2 Tile -> Bool) -> V2 WorldPos -> Bool
hitTile f = f . posToTile


cornersX :: V2 Double -> Int -> V2 WorldPos -> (V2 WorldPos, V2 WorldPos)
cornersX ((/ 2) -> V2 (coerce -> sx) sy) ydir p =
  let sy' :: WorldPos
      sy' = coerce $ sy * fromIntegral ydir
   in (p + V2 (-sx) sy', p + V2 sx sy')


cornersY :: V2 Double -> Int -> V2 WorldPos -> (V2 WorldPos, V2 WorldPos)
cornersY ((/ 2) -> V2 sx (coerce -> sy)) xdir p =
  let sx' :: WorldPos
      sx' = coerce $ sx * fromIntegral xdir
   in (p + V2 sx' (-sy), p + V2 sx' sy)


move :: (V2 WorldPos -> Bool) -> V2 Double -> V2 WorldPos -> V2 Double -> V2 WorldPos
move f sz pos (dpos) = do
  let (V2 xd yd) = fmap (round @_ @Int) $ signum dpos
      subdivs :: Int
      subdivs = ceiling $ norm dpos
  head
    $ drop subdivs
    $ iterate (moveX f sz xd . moveY f sz yd . (+ coerce dpos / fromIntegral subdivs ))
    $ pos


epsilon :: Fractional a => a
epsilon = 0.001


moveX :: (V2 WorldPos -> Bool) -> V2 Double -> Int -> V2 WorldPos -> V2 WorldPos
moveX f sz xdir pos =
  let (l, r) = cornersY sz xdir pos
   in case f l || f r of
        False -> pos
        True ->
          case xdir of
            -1 -> pos & _x .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2) ^. _x)
            0 -> pos -- already in the wall
            1 -> pos & _x .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2 - epsilon) ^. _x)
            _ -> error "very impossible"


moveY :: (V2 WorldPos -> Bool) -> V2 Double -> Int -> V2 WorldPos -> V2 WorldPos
moveY f sz ydir pos =
  let (l, r) = cornersX sz ydir pos
   in case f l || f r of
        False -> pos
        True ->
          case ydir of
            -1 -> pos & _y .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2) ^. _y)
            0 -> pos -- already in the wall
            1 -> pos & _y .~ coerce ((tileToPos (posToTile pos + 1) - coerce sz / 2 - epsilon) ^. _y)
            _ -> error "very impossible"

