{-# LANGUAGE OverloadedLabels #-}

module Game.World where

import Types
import SDL
import Data.Coerce (coerce)
import Data.Foldable (toList)

drawFilledRect :: Color -> Rectangle Int  -> Renderable
drawFilledRect c rect rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap fromIntegral rect

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawWorld :: Resources -> World -> Renderable
drawWorld rs = foldMap (drawLevel rs) . toList . w_levels

debugDrawCollision :: Level -> Renderable
debugDrawCollision lv =
  mconcat $ do
    tile <- tilesOf (l_tilebounds lv)
    case l_hitmap lv tile of
      False -> mempty
      True -> pure $
        drawFilledRect (V4 25 25 25 255)
          $ flip Rectangle tileSize
          $ P $ tileToWorld tile

drawLevel :: Resources -> Level -> Renderable
drawLevel rs lv = mconcat
  [ drawBackgroundColor $ l_bgcolor lv
  , l_tiles lv rs
  ]

tileToWorld :: V2 Tile -> V2 Int
tileToWorld = (* tileSize) . coerce

tilesOf :: Rect Tile -> [V2 Tile]
tilesOf (Rect (V2 x y) (V2 w h)) = do
  dx <- [0 .. w - 1]
  dy <- [0 .. h - 1]
  pure $ V2 (x + dx) (y + dy)

