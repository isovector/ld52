{-# LANGUAGE DataKinds #-}
module Game.Objects.Twinkler where

import qualified Data.Set as S
import           Game.Common
import Data.Hashable (hash)
import Data.Generics.Product (position)

twinkler :: WrappedTexture -> LevelLayer -> V2 Tile -> V2 Bool -> Int -> Object
twinkler wt l tpos@(tileToPos -> pos) flips n = proc oi -> do
  let gs = globalState oi

  let wt' =
        wt & #wt_sourceRect . #_Just . position @1 . #_P %~ \p ->
         p & _x .~ (fromIntegral n - 18) * tileSize
           & _y .~ tileSize

  t <- localTime -< ()
  let seed = traceShowId $ abs $ hash tpos
      rate = 2 + seed `mod` 4
      speed = fromIntegral $ 1 + seed `mod` 5
      onoff = (round (t * speed) + seed) `mod` rate

  returnA -<
    ObjectOutput
      { oo_events = mempty
      , oo_render
          = ifA (S.member l (gs_layerset gs) && onoff == 0)
          $ drawTextureOriginRect wt' (OriginRect tileSize 0) pos 0 flips
      , oo_state = noObjectState pos
          & #os_tags %~ S.insert IsTileEntity
      }

