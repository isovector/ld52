module Game.Objects
  ( renderObjects
  , addObject
  ) where

import qualified Data.Map as M
import Data.Map (Map)
import FRP
import Types
import Data.Monoid


renderObjects :: Map ObjectId Object -> SF FrameInfo Renderable
renderObjects objs = router objs >>> arr (foldMap oo_render)

router :: Map ObjectId Object -> SF FrameInfo (Map ObjectId ObjectOutput)
router objs0 =
  pSwitch
    (\fi -> fmap (ObjectInput noEvent fi, ) )
    objs0
    (arr $ foldMap (uncurry route) . M.toList . snd)
    (\objs f -> router $ appEndo f objs)


route :: ObjectId -> ObjectOutput -> Event (Endo (Map ObjectId Object))
route oid (ObjectOutput kill spawn _) = mconcat $
  [ Endo (M.delete (traceShowId oid)) <$ kill
  , foldMap (Endo . addObject)  <$> spawn
  ]


addObject :: Object -> Map ObjectId Object -> Map ObjectId Object
addObject obj m =
  let oid = traceShowId $ maybe (ObjectId 0) (succ . fst) $ M.lookupMax $ trace (show $ M.keys m) m
   in M.insert oid obj m

