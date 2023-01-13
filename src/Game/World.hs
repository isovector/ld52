module Game.World where

import Data.Set (Set)
import Engine.Types

drawWorld :: Set LevelLayer -> World -> Renderable
drawWorld layers = foldMap (drawLevel layers) . toList . w_levels

drawLevel :: Set LevelLayer -> Level -> Renderable
drawLevel layers lv = mconcat
  [ -- drawBackgroundColor $ l_bgcolor lv
    flip foldMap layers $ \l -> l_tiles lv l
  ]

tilesOf :: Rect Tile -> [V2 Tile]
tilesOf (Rect (V2 x y) (V2 w h)) = do
  dx <- [0 .. w - 1]
  dy <- [0 .. h - 1]
  pure $ V2 (x + dx) (y + dy)

