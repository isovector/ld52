{-# LANGUAGE OverloadedStrings #-}

module Engine.Importer where

import           Control.DeepSeq (force)
import           Control.Lens hiding (Level)
import           Control.Monad.Except
import           Data.Either (partitionEithers)
import           Data.Generics.Labels ()
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           Engine.Drawing
import           Engine.Prelude
import           Engine.Resources
import           Game.Resources (loadWrappedTexture)
import qualified LDtk as LDtk
import           System.FilePath

import {-# SOURCE #-} Game.Objects


ldtkColorToColor :: LDtk.Color -> Color
ldtkColorToColor (LDtk.Color r g b) = V4 r g b 255

loadWorld :: Engine -> FilePath -> IO World
loadWorld e fp = do
  LDtk.loadLDtk fp >>= \case
    Left err -> error err
    Right root -> World <$> parseLevels e root

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
    :: Map Text WrappedTexture
    -> LDtk.Layer
    -> ( CollisionPurpose -> V2 Tile -> Any
       , Renderable
       )
parseLayer !ts_cache l = do
  let !sz = (parseV2 Tile l #__cWid #__cHei)
      !col = force $ rectangularize (coerce sz) (l ^. #intGridCsv)
      {-# NOINLINE col #-}
      !cols = force $ buildCollisionMap sz col
      {-# NOINLINE cols #-}

      !wt = (ts_cache M.! (l ^. #__tilesetRelPath ^. _Just))
            { wt_size = tileSize
            }

      tilemap = buildTileMap wt $ l ^. #autoLayerTiles
  (   cols
    , drawTileMap tilemap
    )
{-# NOINLINE parseLayer #-}



parseEntities :: LDtk.Layer -> ([Text], Map Text Object)
parseEntities l = do
  let es = l ^. #entityInstances
      refset = foldMap getReferencedEntities es
      ref_es = filter (flip S.member refset . view #iid) es
      unref_es = filter (not . flip S.member refset . view #iid) es
      (ref_errs, refs) = partitionEithers $ buildEntities refmap ref_es
      refmap = M.fromList refs

      (unref_errs, unrefs) = partitionEithers $ buildEntities refmap unref_es
  (ref_errs <> unref_errs, M.fromList unrefs)


buildEntities :: Map Text Object -> [LDtk.Entity] -> [Either Text (Text, Object)]
buildEntities refmap es =  do
    e <- es
    let iid = e ^. #iid
    pure $ fmap (iid, ) $
      buildEntity
        (traceFX "spawning: " id $ e ^. #__identifier)
        (fmap (WorldPos . fromIntegral) $ pairToV2 $ e ^. #px)
        (mkPivotOriginRect
          (parseV2 fromIntegral e #width #height)
          (fmap realToFrac $ pairToV2 $ e ^. #__pivot))
        (buildMap $ e ^. #fieldInstances)
        refmap


mkPivotOriginRect :: V2 Double -> V2 Double -> OriginRect Double
mkPivotOriginRect sz off = OriginRect sz $ off * sz


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
  -- Add a little bonus so we don't have weird culling on the edges
  x <- [-2 .. sx + 2]
  y <- [-2 .. sy + 2]
  pure $ cam + V2 x y

getReferencedEntities :: LDtk.Entity -> Set Text
getReferencedEntities e
  = foldMap S.singleton
  . fmap (view #entityIid)
  . mapMaybe (preview $ #_EntityRefValue)
  . concat
  . mapMaybe (preview $ #_ArrayValue)
  . fmap (view #__value)
  $ if e ^. #__identifier == "SpawnTrigger"
      then view #fieldInstances e
      else []


buildTileMap :: WrappedTexture -> [LDtk.Tile] -> Map (V2 Tile) Renderable
buildTileMap wt ts = M.fromListWith (<>) $ do
  t <- ts
  let pos = fmap fromIntegral $ pairToV2 $ t ^. #px
      wt' = wt { wt_sourceRect = Just (Rectangle (P $ fmap fromIntegral $ pairToV2 $ t ^. #src) tileSize)
               }
  pure
    $ (posToTile pos, )
    $ drawSprite wt' pos  0
    $ flipToMirrors
    $ t ^. #tile_flip


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

parseLevels :: Engine -> LDtk.LDtkRoot -> IO (Map Text Level)
parseLevels e root
  = fmap (either (error . mappend "couldn't parse level: " . unlines) id)
  $ runExceptT $ do
    !ts_cache
       <- liftIO
        $ buildTilesetCache e
        $ catMaybes
        $ root ^.. #defs . #tilesets . traverse . #relPath

    fmap (foldMap (uncurry M.singleton))
      $ for (root ^. #levels) $ \lev -> do

        let nm = lev ^. #identifier
            bounds = Rect (parseV2 Pixel lev #worldX #worldY)
                  $ parseV2 Pixel lev #pxWid #pxHei


        let ls = lev ^. #layerInstances
            (errs, ents) = foldMap parseEntities ls
            make_layer
              = except
              . force
              . pure
              . fromMaybe (mempty, mempty)
              . fmap (parseLayer ts_cache)
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

buildTilesetCache :: Engine -> [FilePath] -> IO (Map Text WrappedTexture)
buildTilesetCache e fps = do
  rpath <- resourceRootPath
  fmap M.fromList $ for fps $ \fp -> do
    wt <- loadWrappedTexture e $ rpath </> "tilesets" </> takeFileName fp
    pure (T.pack fp, wt)

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . pure

