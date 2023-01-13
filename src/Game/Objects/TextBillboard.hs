module Game.Objects.TextBillboard where

import           Data.Text (Text)
import qualified Data.Text as T
import           Game.Common
import           SDL (_xyz)

textBillboard :: Maybe Time -> Double -> Color -> Text -> V2 WorldPos -> Object
textBillboard mt sz col txt pos
  = maybe id (\t -> onTimeElapsed t standardDeathResponse) mt
  $ staticObject pos mempty
  $ drawText sz (col ^. _xyz) (T.unpack txt) pos

