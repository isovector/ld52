{-# LANGUAGE OverloadedStrings #-}

module Registry where

import qualified Data.Map as M
import Data.Map (Map)
import Game.Objects.Test
import Data.Text (Text)
import qualified LDtk.Types as LDtk
import Types


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
buildEntity "Player" pos _ = Left "can't load the player yet"
buildEntity "Grenade" pos props = do
  life <- asFloat "Grenade" "Lifetime" props
  pure $ grenade pos $ realToFrac life
buildEntity nm _ _ = Left $ "unregistered entity: " <> nm

