{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Objects
  ( renderObjects
  , addObject
  ) where

import qualified Data.Map as M
import Data.Map (Map)
import FRP
import Types
import Data.Monoid
import Control.Lens ((%~), over, (.~))
import Game.Camera (camera)


renderObjects :: V2 Double -> ObjectMap Object -> SF FrameInfo ScreenRenderable
renderObjects cam0 objs0 = proc fi -> do
  objs <- router objs0 -< fi
  let focuson = M.lookup (om_camera objs) $ om_map objs
  focus <- camera cam0 -< (fi, maybe 0 oo_pos focuson)
  returnA -< foldMap (flip oo_render focus) $ om_map objs


router :: ObjectMap Object -> SF FrameInfo (ObjectMap ObjectOutput)
router objs0 =
  dpSwitch
    (\fi -> fmap (ObjectInput noEvent fi, ) )
    objs0
    ((arr $ foldMap (uncurry route) . M.toList . om_map . snd) >>> notYet)
    (\objs f -> router $ appEndo f objs)


route :: ObjectId -> ObjectOutput -> Event (Endo (ObjectMap Object))
route oid (oo_events -> ObjectEvents {..}) = mconcat $
  [ Endo (#om_map %~ M.delete (traceShowId oid)) <$ oe_die
  , Endo (#om_camera .~ traceShowId oid) <$ oe_focus
  , foldMap (Endo . over #om_map . insertObject)  <$> oe_spawn
  ]

addObject :: a -> ObjectMap a -> ObjectMap a
addObject a = #om_map %~ insertObject a

insertObject :: a -> Map ObjectId a -> Map ObjectId a
insertObject obj m =
  let oid = maybe (ObjectId 0) (succ . fst) $ M.lookupMax m
   in M.insert oid obj m

