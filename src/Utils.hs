module Utils where

import Types

posToTile :: V2 WorldPos -> V2 Tile
posToTile = fmap $ Tile . floor . (/8) . getWorldPos

tileToPos :: V2 Tile -> V2 WorldPos
tileToPos = fmap (WorldPos . fromIntegral . getTile) . (* tileSize)

setGroundOrigin :: WrappedTexture -> WrappedTexture
setGroundOrigin wt =
  let (V2 w h) = wt_size wt
   in wt
        { wt_origin = V2 (div w 2) (h - 6)
        }
