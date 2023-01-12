module Game.Common where

import           Data.Foldable (find)
import qualified Data.Set as S
import           Drawing
import           FRP
import           Types
import Data.Maybe (mapMaybe, listToMaybe)
import Utils (noObjectState, posToTile)
import Data.Monoid

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
    -> (V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObjCallback msg = playerHitRectObj $ \oi ->
  mempty
    { oe_send_message = fmap pure $ msg oi
    }

playerHitRectObj'
    :: (ObjectInput -> ObjectEvents)
    -> OriginRect WorldPos
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj' msg ore col pos =
  playerHitRectObj msg ore (drawOriginRect col ore) pos

playerHitRectObj
    :: (ObjectInput -> ObjectEvents)
    -> OriginRect WorldPos
    -> (V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore r pos =
  proc oi -> do
    let evs = msg oi

    returnA -< ObjectOutput
      { oo_events = evs
      , oo_render = r $ os_pos $ oi_state oi
      , oo_state = (noObjectState pos)
          { os_collision = Just $ coerce ore
          }
      }

getCollisionMap :: GlobalState -> CollisionPurpose -> V2 WorldPos -> Bool
getCollisionMap gs = do
  let lev = gs_currentLevel gs
      layers = gs_layerset gs

  \purpose -> getAny
            . foldMap ((fmap Any .) . l_hitmap lev) layers purpose
            . posToTile


charging :: Time -> SF ObjectInput Bool -> SF ObjectInput (Double, Event Double)
charging dur while = proc oi -> do
  maxed <- after dur 1 -< ()
  t <- sscan (+) 0 -< fi_dt $ oi_frameInfo oi
  x <- while -< oi
  released <- edge -< x
  let prog = t / dur

  let done = mergeEvents
              [ maxed
              , prog <$ released
              ]

  returnA -< (prog , done)

