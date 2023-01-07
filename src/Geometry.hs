module Geometry where

import SDL
import Data.Maybe (isJust)


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
