{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.Objects.Actor (actor)
import           Game.Objects.Player
import           Game.World (drawWorld)
import           SDL
import           Types

#ifndef __HLINT__

initialObjs :: Level -> ObjectMap ObjSF
initialObjs
  = foldr addObject (ObjectMap (ObjectId 0) mempty)
  . l_defaultObjs


dude :: Object
dude
  = Object noObjectMeta
  $ actor (OriginRect 7 (7/2)) (constant 0) (drawPlayer 7) (V2 100 0)


game :: Resources -> SF RawFrameInfo (Camera, Renderable)
game rs = loopPre (initialGlobalState rs) $
  proc (RawFrameInfo c dt , gs) -> do
    let fi = FrameInfo c dt gs
    (cam, objs) <-
      renderObjects rs (V2 0 0)
        -- BUG(sandy): this should be a signal!!!
        (initialObjs $ gs_currentLevel $ initialGlobalState rs)
          -< fi
    bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
    returnA -< ((cam, bg <> objs), gs)

initialGlobalState :: Resources -> GlobalState
initialGlobalState rs
  = GlobalState (w_levels (r_worlds rs TestWorld) M.! "AutoLayer")
  $ S.singleton Layer1

#endif
