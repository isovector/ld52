module Game.Objects.Trampoline where

import Control.Lens ((*~))
import Game.Common


trampoline :: V2 WorldPos -> OriginRect Double -> Double -> Object
trampoline pos ore str
  = onHit (Just . fmap fst) (respondWith $ OnTrampoline str)
  $ staticCollisionObject pos ore mempty
  $ drawGameTextureOriginRect TrampolineTexture draw_ore pos 0
  $ pure False
  where
    draw_ore = ore & #orect_size . _y *~ 2

