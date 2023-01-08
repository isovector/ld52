{-# LANGUAGE OverloadedStrings #-}

module Registry where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Game.Objects.Player (player)
import           Game.Objects.Test
import qualified LDtk.Types as LDtk
import           Game.Objects.Chicken (chicken)
import           Types


asFloat :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Float
asFloat obj field m
  | Just (LDtk.FloatValue v) <- M.lookup field m = Right v
  | otherwise = Left $ mconcat
      [ obj
      , "/"
      , field
      , " was not found"
      ]


buildEntity :: Text -> V2 WorldPos -> Map Text LDtk.FieldValue -> Either Text Object
buildEntity "Player" pos _ = pure $ player pos
buildEntity "Chicken" pos _ = pure $ chicken pos
buildEntity "Grenade" pos props = do
  life <- asFloat "Grenade" "Lifetime" props
  pure $ grenade pos $ realToFrac life
buildEntity nm _ _ = Left $ "unregistered entity: " <> nm

