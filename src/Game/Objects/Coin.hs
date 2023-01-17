module Game.Objects.Coin where

import Game.Common

coin :: V2 WorldPos -> Object
coin pos
  = oscillate (\t -> coerce $ V2 0 (cos (t * 3) * 0.1))
  $ onHitByTag IsPlayer
      (mconcat
        [ standardDeathResponse
        , playSoundReponse CoinSound
        , getCoinResponse
        ])
  $ staticCollisionObject pos ore mempty
  $ \p -> drawGameTextureOriginRect EggTexture (coerce ore) p 0 (pure False)
  where
    ore = OriginRect 8 4
