{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Level where

import           Control.Lens hiding (Level)
import           Data.Aeson (eitherDecodeFileStrict)
import           Data.Coerce (coerce)
import           Data.Generics.Labels ()
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           Drawing
import qualified LDtk.Types as LDtk
import           SDL.Vect hiding (trace)
import           System.FilePath.Lens (basename)
import           Types
import SDL (Rectangle(Rectangle))

ldtkColorToColor :: LDtk.Color -> Color
ldtkColorToColor (LDtk.Color r g b) = V4 r g b 255

loadWorld :: FilePath -> IO World
loadWorld fp = do
  eitherDecodeFileStrict @LDtk.LDtkRoot fp >>= \case
    Left e -> error e
    Right root -> pure $ World $ parseLevels root

buildCollisionMap :: V2 Tile -> [Int] -> V2 Tile -> Any
buildCollisionMap sz csv = trace "building" $ \(coerce -> V2 x y) ->
    Any $ col V.! y V.! x /= 0
  where
    col :: V.Vector (V.Vector Int)
    col = rectangularize (coerce sz) csv

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (here, there) = splitAt n xs
   in here : chunksOf n there

rectangularize :: V2 Int -> [Int] -> V.Vector (V.Vector Int)
rectangularize (V2 x _)
  = V.fromList
  . fmap V.fromList
  . chunksOf x

parseLayer
    :: LDtk.Layer
    -> Either [String] ( V2 Tile -> Any
                       , Resources -> Renderable
                       )
parseLayer l = do
  let cols
        = buildCollisionMap (parseV2 Tile l #__cWid #__cHei)
        $ (l ^. #intGridCsv)
  pure (cols, foldMap (drawTile $ l ^. #__tilesetRelPath) $ l ^. #autoLayerTiles)

drawTile :: Maybe Text -> LDtk.Tile -> Resources -> Renderable
drawTile Nothing _ _ = mempty
drawTile (Just tsstr) t rs = do
  let ts = read @Tileset $ view basename $ T.unpack tsstr
      wt = r_tilesets rs ts
      wt' = wt { wt_sourceRect = Just (Rectangle (P $ fmap fromIntegral $ pairToV2 $ t ^. #src) tileSize)
               , wt_size = tileSize
               }
  drawSprite wt' (fmap fromIntegral $ pairToV2 $ t ^. #px) 0 (flipToMirrors $ t ^. #tile_flip)

flipToMirrors :: LDtk.Flip -> V2 Bool
flipToMirrors LDtk.NoFlip = V2 False False
flipToMirrors LDtk.FlipX = V2 True False
flipToMirrors LDtk.FlipY = V2 False True
flipToMirrors LDtk.FlipXY = V2 True True

pairToV2 :: LDtk.Pair a -> V2 a
pairToV2 (LDtk.Pair x y) = V2 x y


parseV2 :: (a -> b) -> s -> Getting a s a -> Getting a s a -> V2 b
parseV2 ty obj x y = fmap ty $ V2 (obj ^. x) (obj ^. y)


parseLevels :: LDtk.LDtkRoot -> Map Text Level
parseLevels root = either (error . mappend "couldn't parse level: " . unlines) id $
  fmap (foldMap (uncurry M.singleton)) $ for (root ^. #levels) $ \lev -> do
    let nm = lev ^. #identifier
        bounds = Rect (parseV2 Pixel lev #worldX #worldY)
               $ parseV2 Pixel lev #pxWid #pxHei

    !(colmaps, tiles) <-
      fmap unzip
        $ traverse parseLayer
        $ lev ^. #layerInstances

    pure
      ( nm
      , Level
          (ldtkColorToColor $ lev ^. #__bgColor)
          (Rect 0 16)
          bounds
          (mconcat tiles)
          (coerce $ mconcat colmaps)
      )

