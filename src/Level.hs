{-# LANGUAGE OverloadedStrings                  #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Level where

import           Control.DeepSeq (force)
import           Control.Lens hiding (Level)
import           Data.Aeson (eitherDecodeFileStrict)
import           Data.Either (partitionEithers)
import           Data.Generics.Labels ()
import           Data.List (find)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           Drawing
import           Globals (global_tilesets)
import qualified LDtk.Types as LDtk
import           SDL.Vect hiding (trace)
import           System.FilePath.Lens (basename)
import           Types

import {-# SOURCE #-} Registry
import Utils (posToTile)


ldtkColorToColor :: LDtk.Color -> Color
ldtkColorToColor (LDtk.Color r g b) = V4 r g b 255

loadWorld :: FilePath -> IO World
loadWorld fp = do
  eitherDecodeFileStrict @LDtk.LDtkRoot fp >>= \case
    Left e -> error e
    Right root -> pure $ World $ parseLevels root

buildCollisionMap :: V2 Tile -> V.Vector (V.Vector Int) -> CollisionPurpose -> V2 Tile -> Any
buildCollisionMap sz col = \purpose (coerce -> V2 x y) ->
    if x < 0 || y < 0 || x >= sz ^. _x || y >= sz ^. _y
      then Any False
      else Any $ checkPurpose purpose $ col V.! getTile y V.! getTile x

checkPurpose :: CollisionPurpose -> Int -> Bool
checkPurpose _ 0 = False
checkPurpose _ 1 = True
checkPurpose CollisionGround 2 = True
checkPurpose CollisionCheckGround 2 = True
checkPurpose _ 2 = False
checkPurpose CollisionCeiling 3 = True
checkPurpose _ 3 = False
checkPurpose CollisionCheckGround 4 = True
checkPurpose _ 4 = False
checkPurpose _ 5 = False
checkPurpose _ 6 = False
checkPurpose _ i = error $ "unknown tile: " <> show i

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
    -> ( CollisionPurpose -> V2 Tile -> Any
       , Renderable
       )
parseLayer l = do
  let !sz = (parseV2 Tile l #__cWid #__cHei)
      !col = force $ rectangularize (coerce sz) (l ^. #intGridCsv)
      {-# NOINLINE col #-}
      !cols = force $ buildCollisionMap sz col
      {-# NOINLINE cols #-}

      ts = read @Tileset $ view basename $ T.unpack $ l ^. #__tilesetRelPath ^. _Just
      wt = (global_tilesets ts)
            { wt_size = tileSize
            }

      tilemap = buildTileMap wt $ l ^. #autoLayerTiles
  (   cols
    , drawTileMap tilemap
    -- foldMap (drawTile $ l ^. #__tilesetRelPath)
    --    $ l ^. #autoLayerTiles
    )
{-# NOINLINE parseLayer #-}



parseEntities :: LDtk.Layer -> ([Text], [Object])
parseEntities l = partitionEithers $ do
  e <- l ^. #entityInstances
  pure $
    buildEntity
      (traceFX "spawning: " id $ e ^. #__identifier)
      (fmap (WorldPos . fromIntegral) $ pairToV2 $ e ^. #px)
      (parseV2 fromIntegral e #width #height)
      (buildMap $ e ^. #fieldInstances)

buildMap :: [LDtk.Field] -> M.Map Text LDtk.FieldValue
buildMap =
  foldMap $ \x ->
    M.singleton (x ^. #__identifier) (x ^. #__value)


drawTileMap :: Map (V2 Tile) Renderable -> Renderable
drawTileMap tm cam =
  foldMap (maybe mempty ($ cam) . flip M.lookup tm) $ getTilesOnScreen cam

getTilesOnScreen :: Camera -> [V2 Tile]
getTilesOnScreen (Camera (negate -> posToTile -> cam)) = do
  let (V2 sx sy) = posToTile logicalSize
  x <- [0..sx]
  y <- [0..sy]
  pure $ cam + V2 x y


buildTileMap :: WrappedTexture -> [LDtk.Tile] -> Map (V2 Tile) Renderable
buildTileMap wt ts = M.fromListWith (<>) $ do
  t <- ts
  let pos = fmap fromIntegral $ pairToV2 $ t ^. #px
      wt' = wt { wt_sourceRect = Just (Rectangle (P $ fmap fromIntegral $ pairToV2 $ t ^. #src) tileSize)
               }
  pure $ (posToTile pos, ) $ drawSprite wt' pos  0 (flipToMirrors $ t ^. #tile_flip)


drawTile :: Maybe Text -> LDtk.Tile -> Renderable
drawTile Nothing = \_ -> mempty
drawTile (Just tsstr) = \t -> do
  let wt' = wt { wt_sourceRect = Just (Rectangle (P $ fmap fromIntegral $ pairToV2 $ t ^. #src) tileSize)
               }
  drawSprite wt' (fmap fromIntegral $ pairToV2 $ t ^. #px) 0 (flipToMirrors $ t ^. #tile_flip)
  where
    ts = read @Tileset $ view basename $ T.unpack tsstr
    wt = (global_tilesets ts)
          { wt_size = tileSize
          }

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
        (errs, ents) = foldMap parseEntities ls
        make_layer
          = force
          . pure
          . fromMaybe (mempty, mempty)
          . fmap parseLayer
          . getLayerFromLevel ls

    !(c1, d1) <- make_layer Layer1
    !(c2, d2) <- make_layer Layer2
    !(c3, d3) <- make_layer Layer3

    traceM $ unlines $ fmap (T.unpack . mappend "[WARNING] level: ") errs

    pure
      ( nm
      , Level
          (ldtkColorToColor $ lev ^. #__bgColor)
          (Rect 0 16)
          bounds
          (\case
            Layer1 -> d1
            Layer2 -> d2
            Layer3 -> d3
          )
          (coerce . \case
            Layer1 -> c1
            Layer2 -> c2
            Layer3 -> c3
          )
          ents
      )

