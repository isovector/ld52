{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Engine.ObjectRouter
  ( renderObjects
  , addObject
  , addStaticObject
  ) where

import           Control.Lens (at, non)
import           Control.Lens.Lens
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Monoid
import           Engine.Camera (camera, getCameraFocus)
import           Engine.Drawing (playSound)
import           Engine.FRP
import           Engine.Geometry (intersects)
import           Engine.Types
import           Engine.Utils (originRectToRect)
import           Game.GameMessageHandler (handleGameMessage)
import Data.Text (Text)


renderObjects
    :: GlobalState
    -> V2 WorldPos
    -> ObjectMap ObjSF
    -> SF RawFrameInfo (Camera, ObjectMap ObjectOutput, Renderable)
renderObjects gs0 cam0 objs0 = proc fi -> do
  objs <- router gs0 objs0 -< fi
  let focuson = M.lookup (objm_camera_focus objs) $ objm_map objs
  focus <- camera cam0 -< (fi, maybe 0 (getCameraFocus . oo_state) focuson)
  let dat = toList $ objm_map objs
  returnA -<
    ( focus
    , objs
    , flip foldMap dat $ mconcat
       [ renderEvents . oo_events
       , oo_render
       ]
    )

renderEvents :: ObjectEvents -> Renderable
renderEvents oe _ =
  foldMap (foldMap playSound) $ oe_play_sound oe


emptyObjMap :: GlobalState -> ObjectMap a
emptyObjMap gs = ObjectMap
  { objm_camera_focus = StaticId ""  -- TODO(sandy): should be Nothing
  , objm_undeliveredMsgs = mempty
  , objm_globalState = gs
  , objm_map = mempty
  }


router :: GlobalState -> ObjectMap ObjSF -> SF RawFrameInfo (ObjectMap ObjectOutput)
router gs0 om =
  loopPre (emptyObjMap gs0) $
    router' om
      >>> arr dup



router' :: ObjectMap ObjSF -> SF (RawFrameInfo, ObjectMap ObjectOutput) (ObjectMap ObjectOutput)
router' objs0 =
  pSwitch
      @ObjectMap
      @(RawFrameInfo, ObjectMap ObjectOutput)
      @ObjectInput
      @ObjectOutput
      @(Endo (ObjectMap ObjSF))
    (\(fi, outlast) -> routeHits fi outlast)
    (objs0)
    ((arr
        $ foldMap (uncurry route)
        . M.toList
        . objm_map
        . snd
        . fst
     )
     >>> notYet
    )
    -- NOTE(sandy): this only gets called on a new event!!!
    (\new f -> router' $ appEndo f $ new & #objm_undeliveredMsgs .~ mempty)


routeHits :: RawFrameInfo -> ObjectMap ObjectOutput -> ObjectMap sf -> ObjectMap (ObjectInput, sf)
routeHits rfi outlast new = do
  let fi = rfi & #fi_global .~ objm_globalState new
      hittable
        = M.fromList
        $ M.foldMapWithKey (\k m -> maybeToList . sequenceA . (k, ) . fmap (m, ) $ getCollisionRect $ oo_state m)
        $ objm_map outlast
  new
       & #objm_map %~ M.mapWithKey
    (\oid sf -> (, sf) $ ObjectInput
        { oi_self = oid
        , oi_events = mconcat
            [ pushHits oid (fmap (first oo_state) hittable)
            , recv oid $ objm_undeliveredMsgs new
            ]
        , oi_everyone = fmap oo_state $ objm_map outlast
        , oi_frameInfo = fi
        , oi_state
            = maybe noObjectState id
            $ fmap oo_state
            $ M.lookup oid
            $ objm_map outlast
        }
    )

recv :: ObjectId -> Map ObjectId [(ObjectId, Message)] -> ObjectInEvents
recv oid
    = foldMap (\msgs -> mempty & #oie_receive <>~ foldMap (pure . pure) msgs)
    . M.lookup oid


pushHits
    :: ObjectId
    -> Map ObjectId (ObjectState, Rectangle WorldPos)
    -> ObjectInEvents
pushHits oid objs
  | Just me <- M.lookup oid objs
  = foldMap (doHit oid $ snd me) $ M.toList objs
  | otherwise
  = mempty

noObjectState :: ObjectState
noObjectState = ObjectState
  { os_pos = 0
  , os_collision = Nothing
  , os_tags = mempty
  , os_camera_offset = 0
  }


doHit
    :: ObjectId
    -> Rectangle WorldPos
    -> (ObjectId, (ObjectState, Rectangle WorldPos))
    -> ObjectInEvents
doHit me rect (other, (meta, hit))
  | me == other = mempty
  | otherwise
    = mempty
      { oie_hit = fmap (pure . (other, ))
          . maybeToEvent
          $ bool Nothing (Just meta)
          $ intersects rect hit
      }


getCollisionRect :: ObjectState -> Maybe (Rectangle WorldPos)
getCollisionRect os = flip originRectToRect (os_pos os) . coerce <$> os_collision os


route :: ObjectId -> ObjectOutput -> Event (Endo (ObjectMap ObjSF))
route oid (oo_events -> ObjectEvents {..}) = mconcat $
  [ Endo (#objm_map %~ M.delete oid) <$ oe_die
  , Endo (#objm_camera_focus .~ oid) <$ oe_focus
  , foldMap (Endo . over #objm_map . insertObject) <$> oe_spawn
  , Endo <$> oe_omnipotence
  , foldMap (Endo . uncurry (sendMsg oid)) <$> oe_send_message
  , foldMap (Endo . broadcast oid) <$> oe_broadcast_message
  , foldMap (Endo . over (#objm_globalState . #gs_gameState) . handleGameMessage)
      <$> oe_game_message
    -- NOTE(sandy): looks stupid but necessary to flush the pipes
  , Event (Endo id)
  ]

broadcast :: ObjectId -> Message -> ObjectMap ObjSF -> ObjectMap ObjSF
broadcast from m om =
  om
    & #objm_undeliveredMsgs <>~ foldMap (flip M.singleton [(from, m)]) (M.keys $ objm_map om)

sendMsg :: ObjectId -> ObjectId -> Message -> ObjectMap ObjSF -> ObjectMap ObjSF
sendMsg from oid m = #objm_undeliveredMsgs . at oid . non mempty <>~ [(from, m)]


addObject :: a -> ObjectMap a -> ObjectMap a
addObject a = #objm_map %~ insertObject a

addStaticObject :: Text -> a -> ObjectMap a -> ObjectMap a
addStaticObject stid a = #objm_map %~ M.insert (StaticId stid) a


getNextObjId :: ObjectId -> ObjectId
getNextObjId (StaticId _) = DynamicId 0
getNextObjId (DynamicId n) = DynamicId $ succ n

insertObject :: a -> Map ObjectId a -> Map ObjectId a
insertObject obj m =
  let oid = maybe (DynamicId 0) (getNextObjId . fst) $ M.lookupMax m
   in M.insert oid obj m

