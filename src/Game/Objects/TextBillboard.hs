module Game.Objects.TextBillboard where

import Types
import Data.Text (Text)
import FRP
import Utils (noObjectState)
import SDL (_xyz)
import Drawing
import qualified Data.Text as T

textBillboard :: Double -> Color -> Text -> V2 WorldPos -> Object
textBillboard sz col txt pos = constant $ ObjectOutput
  { oo_events = mempty
  , oo_render = drawText sz (col ^. _xyz) (T.unpack txt) pos
  , oo_state = noObjectState pos
  }

