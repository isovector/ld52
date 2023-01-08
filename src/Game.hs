{-# LANGUAGE CPP              #-}

module Game where

import qualified Data.Set as S
import           FRP
import           Game.Objects (renderObjects, addObject)
import           Game.Objects.Actor (actor)
import           Game.Objects.Player
import           Game.Objects.Test
import           Game.World (drawWorld)
import           SDL
import           Types

#ifndef __HLINT__

initialObjs :: Resources -> ObjectMap ObjSF
initialObjs rs
  = addObject (player rs)
  $ addObject (dude rs)
  $ addObject grenade
  $ ObjectMap (ObjectId 0) mempty


dude :: Resources -> Object
dude rs
  = Object noObjectMeta
  $ arr (head $ toList $ w_levels $ r_worlds rs TestWorld ,)
    >>> actor (OriginRect 7 (7/2)) (constant 0) (drawPlayer rs 7) (V2 100 0)


game :: Resources -> SF FrameInfo (Camera, Renderable)
game rs = proc fi -> do
  (cam, objs) <- renderObjects rs (V2 0 0) (initialObjs rs) -< fi
  bg <- constant $ drawWorld rs (S.singleton Layer1) $ r_worlds rs TestWorld -< fi
  returnA -< (cam, bg <> objs)


#endif
