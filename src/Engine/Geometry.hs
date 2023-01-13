module Engine.Geometry where

import SDL
import Data.Maybe (isJust)
import Engine.Types

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


rectContains :: (Ord a, Num a) => Rectangle a -> V2 a -> Bool
rectContains (Rectangle (P (V2 x0 y0)) (V2 w h)) (V2 x y) =
  and
    [ x0 <= x
    , x <= x0 + w
    , y0 <= y
    , y <= y0 + h
    ]


intersects :: (Ord a, Num a) => Rectangle a -> Rectangle a -> Bool
intersects r1 r2 = isJust $ getIntersection r1 r2


getIntersection
    :: (Ord a, Num a) => Rectangle a -> Rectangle a -> Maybe (Rectangle a)
getIntersection r1 r2 =
  let r_x (Rectangle (P (V2 x _)) _) = x
      r_y (Rectangle (P (V2 _ y)) _) = y
      r_w (Rectangle _ (V2 w' _)) = w'
      r_h (Rectangle _ (V2 _ h')) = h'
      x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      w = x1 - x0
      h = y1 - y0
   in case 0 < w && 0 < h of
        True -> Just $ Rectangle (P (V2 x0 y0)) $ V2 w h
        False -> Nothing
