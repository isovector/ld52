{-# LANGUAGE RecordWildCards #-}

module Game.Objects
  ( renderObjects
  , addObject
  ) where

import           Control.Lens (at)
import           Control.Lens.Lens
import           Data.Bool (bool)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Monoid
import           Drawing (playSound)
import           FRP
import           Game.Camera (camera)
import           Geometry (intersects)
import           Types
import           Utils (originRectToRect)


renderObjects
    :: Resources
    -> V2 WorldPos
    -> ObjectMap ObjSF
    -> SF RawFrameInfo (Camera, ObjectMap ObjectOutput, Renderable)
renderObjects rs cam0 objs0 = proc fi -> do
  objs <- router objs0 -< fi
  let focuson = M.lookup (objm_camera_focus objs) $ objm_map objs
  focus <- camera cam0 -< (fi, maybe 0 (os_pos . oo_state) focuson)
  let dat = toList $ objm_map objs
  returnA -<
    ( focus
    , objs
    , flip foldMap dat $ mconcat
       [ renderEvents rs . oo_events
       , oo_render
       ]
    )

renderEvents :: Resources -> ObjectEvents -> Renderable
renderEvents rs oe _ =
  foldMap (foldMap $ playSound rs) $ oe_play_sound oe


emptyObjMap :: ObjectMap a
emptyObjMap =
  ObjectMap (ObjectId 0) (error "emptyObjMap: called global state too soon") mempty


router :: ObjectMap ObjSF -> SF RawFrameInfo (ObjectMap ObjectOutput)
router om =
  loopPre emptyObjMap $
    router' om >>> arr dup


router' :: ObjectMap ObjSF -> SF (RawFrameInfo, ObjectMap ObjectOutput) (ObjectMap ObjectOutput)
router' objs0 =
  dpSwitch
      @ObjectMap
      @(RawFrameInfo, ObjectMap ObjectOutput)
      @ObjectInput
      @ObjectOutput
      @(Endo (ObjectMap ObjSF))
    (\(fi, outs) -> routeHits fi outs )
    objs0
    ((arr
        $ foldMap (uncurry route)
        . M.toList
        . objm_map
        . snd
     ) >>> notYet)
    (\objs f -> router' $ appEndo f objs)


routeHits :: RawFrameInfo -> ObjectMap ObjectOutput -> ObjectMap sf -> ObjectMap (ObjectInput, sf)
routeHits (RawFrameInfo c dt) outs objs = do
  let fi = FrameInfo c dt $ objm_globalState objs
      hittable
        = M.fromList
        $ M.foldMapWithKey (\k m -> maybeToList . sequenceA . (k, ) . fmap (m, ) $ getCollisionRect $ oo_state m)
        $ objm_map outs
  objs & #objm_map %~ M.mapWithKey (pushHits fi $ fmap (first oo_state) hittable)


pushHits
    :: FrameInfo
    -> Map ObjectId (ObjectState, Rectangle WorldPos)
    -> ObjectId
    -> sf
    -> (ObjectInput, sf)
pushHits fi objs oid wm
  | Just me <- M.lookup oid objs
  = (ObjectInput oid (foldMap (doHit oid $ snd me) $ M.toList objs) fi om, wm)
  | otherwise
  = (ObjectInput oid noEvent fi om, wm)
  where
    om = maybe noObjectState fst $ M.lookup oid objs

noObjectState :: ObjectState
noObjectState = ObjectState
  { os_pos = 0
  , os_collision = Nothing
  , os_tags = mempty
  }


doHit
    :: ObjectId
    -> Rectangle WorldPos
    -> (ObjectId, (ObjectState, Rectangle WorldPos))
    -> Event [HitEvent]
doHit me rect (other, (meta, hit))
  | me == other = noEvent
  | otherwise
    = fmap (pure . (other, ))
    . maybeToEvent
    $ bool Nothing (Just meta)
    $ intersects rect hit


getCollisionRect :: ObjectState -> Maybe (Rectangle WorldPos)
getCollisionRect os = flip originRectToRect (os_pos os) . coerce <$> os_collision os


route :: ObjectId -> ObjectOutput -> Event (Endo (ObjectMap ObjSF))
route oid (oo_events -> ObjectEvents {..}) = mconcat $
  [ Endo (#objm_map %~ M.delete oid) <$ oe_die
  , Endo (#objm_camera_focus .~ oid) <$ oe_focus
  , foldMap (Endo . over #objm_map . insertObject)  <$> oe_spawn
  , Endo <$> oe_omnipotence
  , foldMap (Endo . over #objm_map . uncurry sendMsg) <$> oe_send_message
  ]

sendMsg
    :: ObjectId
    -> (ObjectState -> ObjectState)
    -> Map ObjectId (SF ObjectInput ObjectOutput)
    -> Map ObjectId (SF ObjectInput ObjectOutput)
sendMsg oid msg = at oid . #_Just %~ (over #oi_state msg >=-)


addObject :: a -> ObjectMap a -> ObjectMap a
addObject a = #objm_map %~ insertObject a


insertObject :: a -> Map ObjectId a -> Map ObjectId a
insertObject obj m =
  let oid = maybe (ObjectId 0) (succ . fst) $ M.lookupMax m
   in M.insert oid obj m

