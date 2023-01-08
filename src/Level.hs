{-# LANGUAGE OverloadedStrings                  #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Level where

import           Control.Lens hiding (Level)
import           Data.Aeson (eitherDecodeFileStrict)
import           Data.Either (partitionEithers)
import           Data.Generics.Labels ()
import           Data.List (find)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           Drawing
import qualified LDtk.Types as LDtk
import           Registry
import           SDL.Vect hiding (trace)
import           System.FilePath.Lens (basename)
import           Types

ldtkColorToColor :: LDtk.Color -> Color
ldtkColorToColor (LDtk.Color r g b) = V4 r g b 255

loadWorld :: FilePath -> IO World
loadWorld fp = do
  eitherDecodeFileStrict @LDtk.LDtkRoot fp >>= \case
    Left e -> error e
    Right root -> pure $ World $ parseLevels root

buildCollisionMap :: V2 Tile -> [Int] -> V2 Tile -> Any
buildCollisionMap sz csv = \(coerce -> V2 x y) ->
    if x < 0 || y < 0 || x >= sz ^. _x || y >= sz ^. _y
      then Any False
      else Any $ col V.! getTile y V.! getTile x /= 0
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
    -> ( V2 Tile -> Any
       , Resources -> Renderable
       )
parseLayer l = do
  let cols
        = buildCollisionMap (parseV2 Tile l #__cWid #__cHei)
        $ (l ^. #intGridCsv)
  (cols, foldMap (drawTile $ l ^. #__tilesetRelPath) $ l ^. #autoLayerTiles)



parseEntities :: LDtk.Layer -> ([Text], [Object])
parseEntities l = partitionEithers $ do
  e <- l ^. #entityInstances
  pure $
    buildEntity
      (e ^. #__identifier)
      (fmap (WorldPos . fromIntegral) $ pairToV2 $ e ^. #px)
      (buildMap $ e ^. #fieldInstances)

buildMap :: [LDtk.Field] -> M.Map Text LDtk.FieldValue
buildMap =
  foldMap $ \x ->
    M.singleton (x ^. #__identifier) (x ^. #__value)


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

getLayerFromLevel :: [LDtk.Layer] -> LevelLayer -> Maybe (LDtk.Layer)
getLayerFromLevel ls l =
  find ((== T.pack (show l)) . view #__identifier) ls

parseLevels :: LDtk.LDtkRoot -> Map Text Level
parseLevels root = either (error . mappend "couldn't parse level: " . unlines) id $
  fmap (foldMap (uncurry M.singleton)) $ for (root ^. #levels) $ \lev -> do
    let nm = lev ^. #identifier
        bounds = Rect (parseV2 Pixel lev #worldX #worldY)
               $ parseV2 Pixel lev #pxWid #pxHei


    let ls = lev ^. #layerInstances
        ll = fmap parseLayer . getLayerFromLevel ls

        (errs, ents) = foldMap parseEntities ls

    traceM $ unlines $ fmap (T.unpack . mappend "[WARNING] level: ") errs

    pure
      ( nm
      , Level
          (ldtkColorToColor $ lev ^. #__bgColor)
          (Rect 0 16)
          bounds
          (fmap (maybe mempty snd) ll)
          (coerce $ fmap (maybe mempty fst) ll)
          ents
      )

