module Engine.Common where

import           Control.Lens (Lens')
import qualified Data.Set as S
import           Engine.Prelude

response
    :: Lens' ObjectEvents (Event b)
    -> (a -> b)
    -> SF (Event a) ObjectEvents
response field f = arr $ \ev -> mempty & field .~ fmap f ev

listenInbox :: ((ObjectId, Message) -> Maybe a) -> ObjectInEvents -> Event a
listenInbox ok oi = do
  msgs <- oie_receive oi
  case listToMaybe $ mapMaybe ok msgs of
    Just a -> pure a
    Nothing -> noEvent

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
respondWith msg = response #oe_send_message $ fmap (, msg)

