module Game.Tiles where

import Engine.Types
import Game.Objects.Twinkler (twinkler)


handleTileData :: WrappedTexture -> LevelLayer -> V2 Tile -> TileData -> Object
handleTileData wt l tpos (Twinkle n) = twinkler wt l tpos n

