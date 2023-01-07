module Game.World where

import Data.Set (Set)
import Drawing
import Types

drawWorld :: Resources -> Set LevelLayer -> World -> Renderable
drawWorld rs layers = foldMap (drawLevel rs layers) . toList . w_levels

drawLevel :: Resources -> Set LevelLayer -> Level -> Renderable
drawLevel rs layers lv = mconcat
  [ drawBackgroundColor $ l_bgcolor lv
  , flip foldMap layers $ \l -> l_tiles lv l rs
  ]

tileToWorld :: V2 Tile -> V2 Int
tileToWorld = (* tileSize) . coerce

tilesOf :: Rect Tile -> [V2 Tile]
tilesOf (Rect (V2 x y) (V2 w h)) = do
  dx <- [0 .. w - 1]
  dy <- [0 .. h - 1]
  pure $ V2 (x + dx) (y + dy)

