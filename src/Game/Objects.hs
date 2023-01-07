{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Objects
  ( renderObjects
  , addObject
  ) where

import           Control.Lens ((%~), over, (.~))
import           Data.Functor.Compose (getCompose)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           FRP
import           Game.Camera (camera)
import           Types
import SDL (Point(P), Rectangle (Rectangle))
import Data.Maybe (maybeToList)


renderObjects :: V2 WorldPos -> ObjectMap ObjSF -> SF FrameInfo (Camera, Renderable)
renderObjects cam0 objs0 = proc fi -> do
  objs <- router objs0 -< undefined -- fi
  let focuson = M.lookup (objm_camera_focus objs) $ getCompose $ objm_map objs
  focus <- camera cam0 -< (fi, maybe 0 (oo_pos . obj_data) focuson)
  returnA -< (focus, foldMap oo_render . fmap obj_data . getCompose $ objm_map objs)


router :: ObjectMap ObjSF -> SF (FrameInfo, ObjectMap ObjectOutput) (ObjectMap ObjectOutput)
router objs0 =
  dpSwitch
    (\(fi, outs) -> routeHits fi outs  )
    objs0
    ((arr $ foldMap (uncurry route) . M.toList . fmap obj_data . getCompose . objm_map . snd) >>> notYet)
    (\objs f -> router $ appEndo f objs)

routeHits :: FrameInfo -> ObjectMap ObjectOutput -> ObjectMap sf -> ObjectMap (ObjectInput, sf)
routeHits fi outs objs = do
  let hittable
        = M.foldMapWithKey (\k -> maybeToList . sequenceA . (k, ) . getCollisionRect)
        $ getCompose
        $ objm_map outs
  undefined


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

