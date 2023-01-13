module Game.Objects.Trampoline where

import Control.Lens ((*~))
import Game.Common


trampoline :: V2 WorldPos -> V2 Double -> Double -> Object
trampoline pos (flip OriginRect 0 -> ore) str
  = onHit (Just . fmap fst) (respondWith $ OnTrampoline str)
  $ staticCollisionObject pos ore mempty
  $ drawGameTextureOriginRect TrampolineTexture draw_ore pos 0
  $ pure False
  where
    draw_ore = ore & #orect_size . _y *~ 2

