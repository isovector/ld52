module Game.Objects.TextBillboard where

import Types
import Data.Text (Text)
import FRP
import Utils (noObjectState)
import SDL (_xyz)
import Drawing
import qualified Data.Text as T

textBillboard :: Maybe Time -> Double -> Color -> Text -> V2 WorldPos -> Object
textBillboard mt sz col txt pos = proc _ -> do
  die <- maybe never (flip after ()) mt -< ()
  returnA -< ObjectOutput
    { oo_events = mempty
        { oe_die = die
        }
    , oo_render = drawText sz (col ^. _xyz) (T.unpack txt) pos
    , oo_state = noObjectState pos
    }

