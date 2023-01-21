module Game.Objects.Coin where

import Game.Common

coin :: V2 WorldPos -> Object
coin pos
  = onHitByTag IsPlayer
      (mconcat
        [ standardDeathResponse
        , playSoundReponse CoinSound
        , getCoinResponse
        ])
  $ oscillatingStaticCollisionObject (\t -> coerce $ V2 0 (cos (t * 4) * 4)) pos ore mempty
  $ \p -> drawGameTextureOriginRect EggTexture (coerce ore) p 0 (pure False)
  where
    ore = OriginRect 8 4
