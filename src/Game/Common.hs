module Game.Common where

import           Data.Foldable (find)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Types
import Data.Maybe (mapMaybe, listToMaybe)
import Utils (noObjectState)

onHitBy :: ObjectTag -> ObjectInput -> Event ObjectId
onHitBy otag oi = do
  hits <- oie_hit $ oi_events oi
  case find (any $ S.member otag . os_tags) hits of
    Just x0 -> pure $ fst x0
    Nothing -> noEvent

listenInbox :: (Message -> Maybe a) -> ObjectInEvents -> Event a
listenInbox ok oi = do
  msgs <- oie_receive oi
  case listToMaybe $ mapMaybe ok msgs of
    Just a -> pure a
    Nothing -> noEvent

playerHitRectObjCallback
    :: (ObjectInput -> Event (ObjectId, Message))
    -> OriginRect WorldPos
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObjCallback msg = playerHitRectObj $ \oi ->
  mempty
    { oe_send_message = fmap pure $ msg oi
    }

playerHitRectObj
    :: (ObjectInput -> ObjectEvents)
    -> OriginRect WorldPos
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore col pos =
  proc oi -> do
    let evs = msg oi

    returnA -< ObjectOutput
      { oo_events = evs
      , oo_render =
          drawOriginRect col ore pos
      , oo_state = (noObjectState pos)
          { os_collision = Just $ coerce ore
          }
      }

