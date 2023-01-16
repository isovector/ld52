module Game.Tiles where

import Engine.Types
import Game.Objects.Twinkler (twinkler)


handleTileData :: WrappedTexture -> LevelLayer -> V2 Tile -> V2 Bool -> TileData -> Object
handleTileData wt l tpos flips (Twinkle n) = twinkler wt l tpos flips n

