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


renderObjects :: V2 WorldPos -> ObjectMap ObjSF -> SF FrameInfo (Camera, Renderable)
renderObjects cam0 objs0 = proc fi -> do
  objs <- router objs0 -< fi
  let focuson = M.lookup (om_camera_focus objs) $ getCompose $ om_map objs
  focus <- camera cam0 -< (fi, maybe 0 (oo_pos . obj_data) focuson)
  returnA -< (focus, foldMap oo_render . fmap obj_data . getCompose $ om_map objs)


router :: ObjectMap ObjSF -> SF FrameInfo (ObjectMap ObjectOutput)
router objs0 =
  dpSwitch
    (\fi -> fmap (ObjectInput noEvent fi, ) )
    objs0
    ((arr $ foldMap (uncurry route) . M.toList . fmap obj_data . getCompose . om_map . snd) >>> notYet)
    (\objs f -> router $ appEndo f objs)


route :: ObjectId -> ObjectOutput -> Event (Endo (ObjectMap ObjSF))
route oid (oo_events -> ObjectEvents {..}) = mconcat $
  [ Endo (#om_map . #_Compose %~ M.delete oid) <$ oe_die
  , Endo (#om_camera_focus .~ oid) <$ oe_focus
  , foldMap (Endo . over (#om_map . #_Compose) . insertObject)  <$> oe_spawn
  ]


addObject :: ObjectMeta a -> ObjectMap a -> ObjectMap a
addObject a = #om_map . #_Compose %~ insertObject a


insertObject :: a -> Map ObjectId a -> Map ObjectId a
insertObject obj m =
  let oid = maybe (ObjectId 0) (succ . fst) $ M.lookupMax m
   in M.insert oid obj m

