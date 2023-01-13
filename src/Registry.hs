{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Registry where

import           Control.Applicative (optional)
import           Control.Error (note)
import           Control.Lens (Prism')
import           Control.Monad.Error (Error)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Game.Objects.Checkpoint (checkpoint)
import           Game.Objects.Chicken (chicken)
import           Game.Objects.Coin (coin)
import           Game.Objects.CollectPowerup (collectPowerup)
import           Game.Objects.Death (deathZone)
import           Game.Objects.Door (door)
import           Game.Objects.ParticleSpawner (particleSpawner)
import           Game.Objects.Player (player)
import           Game.Objects.SpawnTrigger (spawnTrigger)
import           Game.Objects.Test
import           Game.Objects.TextBillboard (textBillboard)
import           Game.Objects.ToggleRegion (toggleRegion)
import           Game.Objects.Trampoline (trampoline)
import           Game.Objects.Unknown (unknown)
import qualified LDtk.Types as LDtk
import           Level (ldtkColorToColor)
import           Engine.Types
import           Utils (tileToPos)


buildEntity
    :: Text
    -> V2 WorldPos
    -> V2 Double
    -> Map Text LDtk.FieldValue
    -> Map Text Object
    -> Either Text Object
buildEntity "Player" pos _ _ _ = pure $ player pos
buildEntity "PowerUp" pos _ props _ =
  collectPowerup pos
    <$> asEnum "PowerUp" "power" props
buildEntity "Trampoline" pos sz props _ =
  trampoline pos sz
    <$> asDouble "Trampoline" "strength" props
buildEntity "Chicken" pos _ _ _ = pure $ chicken pos
buildEntity "Checkpoint" pos _ _ _ = pure $ checkpoint pos
buildEntity "Door" pos sz props _ =
  door pos sz
    <$> asPos "Door" "out" props
buildEntity "Coin" pos _ _ _ = pure $ coin pos
buildEntity "Death" pos sz _ _ = pure $ deathZone pos sz
buildEntity "ToggleLayer" pos sz props _ =
  toggleRegion pos
    <$> pure sz
    <*> asEnum "Toggle" "layer" props
    <*> asBool "Toggle" "toggle" props
buildEntity "Text" pos _ props _ =
  textBillboard
    <$> optional (asDouble "Text" "duration" props)
    <*> asDouble "Text" "size" props
    <*> asColor "Text" "color" props
    <*> asText "Text" "text" props
    <*> pure pos
buildEntity "Grenade" pos _ props _ = do
  life <- asFloat "Grenade" "Lifetime" props
  pure $ grenade pos $ realToFrac life
buildEntity "ParticleSpawner" pos _ props _ = do
  pt <- asEnum "ParticleSpawner" "type" props
  pure $ particleSpawner pos pt
buildEntity "SpawnTrigger" pos sz props refmap = do
  persistent <- asBool "SpawnTrigger" "persistent" props
  refs <- getRefs "SpawnTrigger" "refs" props refmap
  pure $ spawnTrigger pos (coerce sz) persistent refs
buildEntity nm pos sz _ _ = do
  traceM $ "unregistered entity: " <> T.unpack nm
  pure $ unknown nm pos sz

instance Error Text where


as :: Text -> (Prism' LDtk.FieldValue a) -> Text -> Text -> Map Text LDtk.FieldValue -> Either Text a
as ty pris obj field m
  | Just (preview pris -> Just v) <- M.lookup field m = Right v
  | Just v <- M.lookup field m = Left $ mconcat
      [ obj
      , "/"
      , field
      , " had the wrong type ("
      , T.pack $ show v
      , ") but wanted "
      , ty
      ]
  | otherwise = Left $ mconcat
      [ obj
      , "/"
      , field
      , " was not found"
      ]

getRefs :: Text -> Text -> Map Text LDtk.FieldValue -> Map Text Object -> Either Text [Object]
getRefs obj prop props refs = do
  z <- as "Array" #_ArrayValue obj prop props
  iids <- for z $ note "couldn't lookup ref" . fmap (view #entityIid) . preview #_EntityRefValue
  pure $ foldMap (pure . (refs M.!)) iids

asPos :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text (V2 WorldPos)
asPos = fmap (fmap (fmap gridToWorld)) . as "Text" #_PointValue

gridToWorld :: LDtk.GridPoint -> V2 WorldPos
-- TODO(sandy): EXTREME HACK
-- the editor gives us this coordinate in CURRENT GRID SIZE
-- which is 8 lol (half of the tile size)
gridToWorld (LDtk.GridPoint cx cy) = (/ 2) $ tileToPos $ coerce $ V2 cx cy

asText :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Text
asText = as "Text" #_StringValue

asDouble :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Double
asDouble = fmap (fmap (fmap realToFrac)) . asFloat

asFloat :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Float
asFloat = as "Float" #_FloatValue

asColor :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Color
asColor = fmap (fmap (fmap ldtkColorToColor)) . as "Color" #_ColorValue

asEnum :: Read a => Text -> Text -> Map Text LDtk.FieldValue -> Either Text a
asEnum = fmap (fmap (fmap $ read . T.unpack)) . as "Enum" #_EnumValue

asBool :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Bool
asBool = as "Int" #_BooleanValue

asInt :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Integer
asInt = as "Int" #_IntegerValue

