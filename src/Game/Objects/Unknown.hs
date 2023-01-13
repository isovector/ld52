module Game.Objects.Unknown where

import qualified Data.Text as T
import           Game.Common


unknown :: Text -> V2 WorldPos -> V2 Double -> Object
unknown tx pos sz =
  staticObject pos mempty $ mconcat
    [ drawOriginRect (V4 255 255 255 128) (coerce $ OriginRect sz 0) pos
    , drawText 3 (V3 255 255 255) (T.unpack tx) (pos - V2 0 5)
    ]


