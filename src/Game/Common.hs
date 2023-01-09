module Game.Common where

import           Data.Foldable (find)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Types
import Data.Maybe (mapMaybe, listToMaybe)

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


playerHitRectObj
    :: (ObjectInput -> Event [(ObjectId, Message)])
    -> OriginRect WorldPos
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore col pos =
  proc oi -> do
    let on_hit = msg oi

    returnA -< ObjectOutput
      { oo_events = mempty
          { oe_send_message = on_hit
          }
      , oo_render =
          drawOriginRect col ore pos
      , oo_state = ObjectState
          { os_pos = pos
          , os_collision = Just $ coerce ore
          , os_tags = mempty
          }
      }

