module Game.Objects.Twinkler where

import qualified Data.Set as S
import           Game.Common

twinkler :: WrappedTexture -> LevelLayer -> V2 Tile -> Int -> Object
twinkler wt l (tileToPos -> pos) _ = proc oi -> do
  let gs = globalState oi
  oo <- staticObject pos (S.singleton IsTileEntity)
      $ drawOriginRect (V4 255 255 255 64) (OriginRect tileSize 0) pos
          -< oi
  returnA -< oo & #oo_render %~ ifA (S.member l $ gs_layerset gs)

