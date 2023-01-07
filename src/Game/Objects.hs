{-# LANGUAGE RecordWildCards #-}

module Game.Objects
  ( renderObjects
  , addObject
  ) where

import           Control.Lens.Lens
import           Data.Bool (bool)
import           Data.Functor.Compose (getCompose)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Monoid
import           Drawing (playSound)
import           FRP
import           Game.Camera (camera)
import           Geometry (intersects)
import           Types


renderObjects
    :: Resources
    -> V2 WorldPos
    -> ObjectMap ObjSF
    -> SF FrameInfo (Camera, Renderable)
renderObjects rs cam0 objs0 = proc fi -> do
  objs <- router objs0 -< fi
  let focuson = M.lookup (objm_camera_focus objs) $ getCompose $ objm_map objs
  focus <- camera cam0 -< (fi, maybe 0 (oo_pos . obj_data) focuson)
  let dat = toList $ fmap obj_data . getCompose $ objm_map objs
  returnA -< (focus, flip foldMap dat (renderEvents rs . oo_events <> oo_render))

renderEvents :: Resources -> ObjectEvents -> Renderable
renderEvents rs oe _ _ =
  foldMap (foldMap $ playSound rs) $ oe_play_sound oe

emptyObjMap :: ObjectMap a
emptyObjMap = ObjectMap (ObjectId 0) $ mempty

router :: ObjectMap ObjSF -> SF FrameInfo (ObjectMap ObjectOutput)
router om =
  loopPre emptyObjMap $
    router' om >>> arr dup

router' :: ObjectMap ObjSF -> SF (FrameInfo, ObjectMap ObjectOutput) (ObjectMap ObjectOutput)
router' objs0 =
  dpSwitch
    (\(fi, outs) -> routeHits fi outs  )
    objs0
    ((arr $ foldMap (uncurry route) . M.toList . fmap obj_data . getCompose . objm_map . snd) >>> notYet)
    (\objs f -> router' $ appEndo f objs)


routeHits :: FrameInfo -> ObjectMap ObjectOutput -> ObjectMap sf -> ObjectMap (ObjectInput, sf)
routeHits fi outs objs = do
  let hittable
        = M.fromList
        $ M.foldMapWithKey (\k m -> maybeToList . sequenceA . (k, ) . fmap (obj_metadata m, ) $ getCollisionRect m)
        $ getCompose
        $ objm_map outs
  objs & #objm_map . #_Compose %~ M.mapWithKey (pushHits fi hittable)


pushHits
    :: FrameInfo
    -> Map ObjectId (ObjectMeta, Rectangle WorldPos)
    -> ObjectId
    -> WithMeta sf
    -> WithMeta (ObjectInput, sf)
pushHits fi hittable oid wm
  | Just me <- M.lookup oid hittable
  = fmap (ObjectInput (foldMap (doHit oid $ snd me) $ M.toList hittable) fi,) wm
  | otherwise
  = fmap (ObjectInput noEvent fi,) wm


doHit :: ObjectId -> Rectangle WorldPos -> (ObjectId, (ObjectMeta, Rectangle WorldPos)) -> Event [HitEvent]
doHit me rect (other, (meta, hit))
  | me == other = noEvent
  | otherwise
    = fmap (pure . (other, ))
    . maybeToEvent
    $ bool Nothing (Just meta)
    $ intersects rect hit


getCollisionRect :: WithMeta ObjectOutput -> Maybe (Rectangle WorldPos)
getCollisionRect (Object om oo) = Rectangle (P $ oo_pos oo) . coerce <$> om_hitSize om


route :: ObjectId -> ObjectOutput -> Event (Endo (ObjectMap ObjSF))
route oid (oo_events -> ObjectEvents {..}) = mconcat $
  [ Endo (#objm_map . #_Compose %~ M.delete oid) <$ oe_die
  , Endo (#objm_camera_focus .~ oid) <$ oe_focus
  , foldMap (Endo . over (#objm_map . #_Compose) . insertObject)  <$> oe_spawn
  ]


addObject :: WithMeta a -> ObjectMap a -> ObjectMap a
addObject a = #objm_map . #_Compose %~ insertObject a


insertObject :: a -> Map ObjectId a -> Map ObjectId a
insertObject obj m =
  let oid = maybe (ObjectId 0) (succ . fst) $ M.lookupMax m
   in M.insert oid obj m

