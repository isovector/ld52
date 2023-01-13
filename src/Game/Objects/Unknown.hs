module Game.Objects.Unknown where

import Game.Common (onHitBy, playerHitRectObj')
import Types
import FRP
import Utils (noObjectState)
import Drawing (drawOriginRect, drawText)
import Data.Text (Text)
import qualified Data.Text as T


unknown :: Text -> V2 WorldPos -> V2 Double -> Object
unknown tx pos sz = arr $ const $ ObjectOutput
  { oo_events = mempty
  , oo_render = mconcat
      [ drawOriginRect (V4 255 255 255 128) (coerce $ OriginRect sz 0) pos
      , drawText 3 (V3 255 255 255) (T.unpack tx) (pos - V2 0 5)
      ]
  , oo_state = noObjectState pos
  }


