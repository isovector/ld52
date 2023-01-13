module Game.Common
  ( module Engine.Prelude
  , module Game.Common
  ) where

import           Data.Monoid
import qualified Data.Set as S
import           Engine.Prelude


onHitBy :: ObjectTag -> ObjectInput -> Event ObjectId
onHitBy otag oi = do
  hits <- oie_hit $ oi_events oi
  case find (any $ S.member otag . os_tags) hits of
    Just x0 -> pure $ fst x0
    Nothing -> noEvent

listenInbox :: ((ObjectId, Message) -> Maybe a) -> ObjectInEvents -> Event a
listenInbox ok oi = do
  msgs <- oie_receive oi
  case listToMaybe $ mapMaybe ok msgs of
    Just a -> pure a
    Nothing -> noEvent

playerHitRectObjCallback
    :: (ObjectInput -> Event (ObjectId, Message))
    -> OriginRect Double
    -> (V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObjCallback msg ore r =
  playerHitRectObj
    (arr $ \oi ->
      (, ()) $ mempty
        { oe_send_message = fmap pure $ msg oi
        })
    ore
    (const r)

playerHitRectObj'
    :: (SF ObjectInput ObjectEvents)
    -> OriginRect Double
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj' msg ore col pos =
  playerHitRectObj (msg >>> arr (, ())) ore (const $ drawOriginRect col ore) pos

playerHitRectObj
    :: (SF ObjectInput (ObjectEvents, a))
    -> OriginRect Double
    -> (a -> V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore r pos =
  proc oi -> do
    (evs, a) <- msg -< oi

    returnA -< ObjectOutput
      { oo_events = evs
      , oo_render = r a $ os_pos $ oi_state oi
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

on :: (Message -> Maybe a) -> SF (Event a) ObjectEvents -> Object -> Object
on msg handle obj =
  proc oi -> do
    let ev = listenInbox (msg . snd) $ oi_events oi
    oo <- obj -< oi
    evs <- handle -< ev
    returnA -< oo & #oo_events <>~ evs

onSpawn :: SF (Event ()) ObjectEvents -> Object -> Object
onSpawn sf obj =
  proc oi -> do
    ev <- nowish () -< ()
    oo <- obj -< oi
    evs <- sf -< ev
    returnA -< oo & #oo_events <>~ evs

onHit :: ([HitEvent] -> Maybe a) -> SF (Event a) ObjectEvents -> Object -> Object
onHit ot handle obj =
  proc oi -> do
    let ev = (>>= maybeToEvent . ot) $ oie_hit $ oi_events oi
    oo <- obj -< oi
    evs <- handle -< ev
    returnA -< oo & #oo_events <>~ evs

onHitByTag :: ObjectTag -> SF (Event HitEvent) ObjectEvents -> Object -> Object
onHitByTag ot = onHit $ find $ any $ S.member ot . os_tags


playSoundReponse :: Sound -> SF (Event a) ObjectEvents
playSoundReponse s = arr $ \ev -> mempty & #oe_play_sound .~ ([s] <$ ev)


standardDeathResponse :: SF (Event a) ObjectEvents
standardDeathResponse = arr $ \ev -> mempty & #oe_die .~ (() <$ ev)


spawnResponse :: [Object] -> SF (Event a) ObjectEvents
spawnResponse objs = arr $ \ev -> mempty & #oe_spawn .~ (objs <$ ev)

omnipotenceResponse :: (ObjectMap Object -> ObjectMap Object) -> SF (Event a) ObjectEvents
omnipotenceResponse objs = arr $ \ev -> mempty & #oe_omnipotence .~ (objs <$ ev)

onDeath :: SF (Event ()) ObjectEvents -> Object -> Object
onDeath = on $ preview #_Die


onTimeElapsed :: Time -> SF (Event ()) ObjectEvents -> Object -> Object
onTimeElapsed dur handle obj =
  proc oi -> do
    ev <- after dur () -< ()
    oo <- obj -< oi
    evs <- handle -< ev
    returnA -< oo & #oo_events <>~ evs


staticCollisionObject
    :: V2 WorldPos
    -> OriginRect Double
    -> S.Set ObjectTag
    -> Renderable
    -> Object
staticCollisionObject pos ore tags r = constant $
  ObjectOutput
    { oo_events = mempty
    , oo_render = r
    , oo_state = (noObjectState pos)
        { os_collision = Just ore
        , os_tags = tags
        }
    }

staticObject
    :: V2 WorldPos
    -> S.Set ObjectTag
    -> Renderable
    -> Object
staticObject pos tags r = constant $
  ObjectOutput
    { oo_events = mempty
    , oo_render = r
    , oo_state = noObjectState pos & #os_tags .~ tags
    }

respondWith :: Message -> SF (Event [ObjectId]) ObjectEvents
respondWith msg = arr $ \ev ->
  mempty & #oe_send_message .~ (fmap (fmap (, msg)) ev)

