module Engine.Utils where

import Engine.FRP
import Engine.Geometry (orTopLeft)
import Engine.Types

nowish :: a -> SF x (Engine.Types.Event a)
nowish a = after 0.016 a

posToTile :: V2 WorldPos -> V2 Tile
posToTile = fmap $ Tile . floor . (/ tileSize) . getWorldPos

tileToPos :: V2 Tile -> V2 WorldPos
tileToPos = fmap (WorldPos . fromIntegral . getTile) . (* tileSize)

setGroundOrigin :: WrappedTexture -> WrappedTexture
setGroundOrigin wt =
  let (V2 w h) = wt_size wt
   in wt
        { wt_origin = V2 (div w 2) h
        }

setCenterOrigin :: WrappedTexture -> WrappedTexture
setCenterOrigin wt =
  let (V2 w h) = wt_size wt
   in wt
        { wt_origin = V2 (div w 2) (div h 2)
        }


wrappedToOriginRect :: WrappedTexture -> OriginRect Double
wrappedToOriginRect wt = fmap fromIntegral $ OriginRect
  { orect_size = wt_size wt
  , orect_offset = wt_origin wt
  }


mkCenterdOriginRect :: Fractional a => V2 a -> OriginRect a
mkCenterdOriginRect sz = OriginRect sz (sz / 2)

mkGroundOriginRect :: Fractional a => V2 a -> OriginRect a
mkGroundOriginRect sz@(V2 x y) = OriginRect sz $ V2 (x / 2) y


originRectToRect :: Num a => OriginRect a -> V2 a -> Rectangle a
originRectToRect ore pos =
  Rectangle (P $ orTopLeft pos ore)
    $ orect_size ore

noObjectState :: V2 WorldPos -> ObjectState
noObjectState pos = ObjectState pos Nothing mempty 0

unlessNull :: [a] -> Maybe [a]
unlessNull [] = Nothing
unlessNull a = Just a

