{-# LANGUAGE OverloadedStrings #-}

module Game.Objects where

import Data.Map (Map)
import Data.Text (Text)
import qualified LDtk.Types as LDtk
import Engine.Types


buildEntity
    :: Text
    -> V2 WorldPos
    -> OriginRect Double
    -> Map Text LDtk.FieldValue
    -> Map Text Object
    -> Either Text Object


