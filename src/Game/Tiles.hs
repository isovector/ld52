module Game.Tiles where

import Engine.Types
import Game.Objects.Twinkler (twinkler)


handleTileData :: WrappedTexture -> LevelLayer -> V2 WorldPos -> V2 Bool -> TileData -> Object
handleTileData wt l pos flips (Twinkle n) = twinkler wt l pos flips n

