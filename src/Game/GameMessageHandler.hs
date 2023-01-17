module Game.GameMessageHandler where

import qualified Data.Set as S
import Engine.Types

handleGameMessage :: GameMessage -> GameState -> GameState
handleGameMessage AddCoin =
  #gs_coins +~ 1
handleGameMessage (AddInventory pt) =
  #gs_inventory <>~ S.singleton pt
handleGameMessage GameWon =
  #gs_end .~ True

