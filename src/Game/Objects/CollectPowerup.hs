module Game.Objects.CollectPowerup where

import           Control.Lens ((<>~))
import qualified Data.Set as S
import           FRP
import           Game.Common (onHitBy, playerHitRectObj)
import           Types


collectPowerup :: V2 WorldPos -> PowerupType -> Object
collectPowerup pos pt =
  playerHitRectObj
    (\oi ->
      let ev = onHitBy IsPlayer oi
       in mempty
            { oe_die = () <$ ev
            , oe_play_sound = [CoinSound] <$ ev
            }
    )
    (OriginRect 8 4)
    (V4 255 255 0 255)
    pos
  >>> arr (#oo_state . #os_tags <>~ S.singleton (IsPowerup pt))


