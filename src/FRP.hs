module FRP
  ( module FRP
  , module FRP.Yampa
  ) where

import FRP.Yampa
import Control.Monad.Cont

newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

swont :: SF a (b, Event c) -> Swont a b c
swont = Swont . cont . switch


dswont :: SF a (b, Event c) -> Swont a b c
dswont = Swont . cont . dSwitch


waitFor :: SF a (Event c) -> SF a b -> Swont a b c
waitFor ev sf = swont $ (,) <$> sf <*> ev

waitForEdge :: (a -> Bool) -> SF a b -> Swont a b ()
waitForEdge f = waitFor (arr f >>> edge)


timed :: Double -> SF a b -> Swont a b ()
timed dur sf = waitFor (after dur ()) sf


lerpSF :: Double -> SF Double b -> Swont a b ()
lerpSF dur sf = timed dur $ localTime >>> arr (/ dur) >>> sf


runSwont :: (a -> SF i o) -> Swont i o a -> SF i o
runSwont end sw = runCont (runSwont' sw) end

