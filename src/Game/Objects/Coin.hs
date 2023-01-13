module Game.Objects.Coin where

import Game.Common

coin :: V2 WorldPos -> Object
coin pos
  = onHitByTag IsPlayer
      (mconcat
        [ standardDeathResponse
        , playSoundReponse CoinSound
        ])
  $ staticCollisionObject pos ore mempty
  $ drawOriginRect (V4 255 255 0 255) (coerce ore) pos
  where
    ore = OriginRect 8 4
