{-# LANGUAGE OverloadedStrings #-}

module Registry where

import Data.Map (Map)
import Data.Text (Text)
import qualified LDtk.Types as LDtk
import Types


buildEntity :: Text -> V2 WorldPos -> Map Text LDtk.FieldValue -> Either Text Object


