module Utils where

import Types
import FRP

nowish :: a -> SF x (Types.Event a)
nowish a = after 0.016 a

posToTile :: V2 WorldPos -> V2 Tile
posToTile = fmap $ Tile . floor . (/8) . getWorldPos

tileToPos :: V2 Tile -> V2 WorldPos
tileToPos = fmap (WorldPos . fromIntegral . getTile) . (* tileSize)

setGroundOrigin :: WrappedTexture -> WrappedTexture
setGroundOrigin wt =
  let (V2 w h) = wt_size wt
   in wt
        { wt_origin = V2 (div w 2) h
        }

focusOn :: SF ObjectOutput ObjectOutput
focusOn = proc oo -> do
  ev <- nowish () -< ()
  returnA -< oo & #oo_events . #oe_focus .~ ev


wrappedToOriginRect :: WrappedTexture -> OriginRect Double
wrappedToOriginRect wt = fmap fromIntegral $ OriginRect
  { orect_size = wt_size wt
  , orect_offset = wt_origin wt
  }

