{-# LANGUAGE OverloadedStrings #-}

module Registry where

import           Control.Lens (Prism', preview)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Game.Objects.Chicken (chicken)
import           Game.Objects.Player (player)
import           Game.Objects.Test
import           Game.Objects.TextBillboard (textBillboard)
import           Game.Objects.ToggleRegion (toggleRegion)
import qualified LDtk.Types as LDtk
import           Level (ldtkColorToColor)
import           Types


buildEntity :: Text -> V2 WorldPos -> V2 Double -> Map Text LDtk.FieldValue -> Either Text Object
buildEntity "Player" pos _ _ = pure $ player pos
buildEntity "Chicken" pos _ _ = pure $ chicken pos
buildEntity "ToggleLayer" pos sz props =
  toggleRegion pos
    <$> pure sz
    <*> asEnum "Toggle" "layer" props
    <*> asBool "Toggle" "toggle" props
buildEntity "Text" pos _ props =
  textBillboard
    <$> traceShowId (asDouble "Text" "size" props)
    <*> asColor "Text" "color" props
    <*> asText "Text" "text" props
    <*> pure pos
buildEntity "Grenade" pos _ props = do
  life <- asFloat "Grenade" "Lifetime" props
  pure $ grenade pos $ realToFrac life
buildEntity nm _ _ _  = Left $ "unregistered entity: " <> nm


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

