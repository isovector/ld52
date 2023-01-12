module Game.Objects.Trampoline where

import Game.Common (onHitBy, playerHitRectObjCallback)
import Types
import Drawing (drawSpriteOriginRect, drawOriginRect)
import Globals (global_textures)
import Control.Lens ((*~))


trampoline :: V2 WorldPos -> V2 Double -> Double -> Object
trampoline pos (flip OriginRect 0 . coerce -> ore) str =
  playerHitRectObjCallback
    (fmap (, OnTrampoline str) . onHitBy IsPlayer)
    ore
    (\p -> mconcat
      [ drawSpriteOriginRect (global_textures TrampolineTexture) draw_ore p 0 $ pure False
      ]
    )
    pos
  where
    draw_ore = ore & #orect_size . _y *~ 2

