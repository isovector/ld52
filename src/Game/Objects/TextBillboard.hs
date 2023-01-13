module Game.Objects.TextBillboard where

import qualified Data.Text as T
import           Game.Common

textBillboard :: Maybe Time -> Double -> Color -> Text -> V2 WorldPos -> Object
textBillboard mt sz col txt pos
  = maybe id (\t -> onTimeElapsed t standardDeathResponse) mt
  $ staticObject pos mempty
  $ drawText sz (col ^. _xyz) (T.unpack txt) pos

