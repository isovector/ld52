{-# OPTIONS_GHC -Wno-orphans #-}
module FRP
  ( module FRP
  , module FRP.Yampa
  ) where

import Control.Monad.Cont
import Data.Monoid
import FRP.Yampa
import Data.Bifunctor
import Data.Tuple (swap)

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

deriving via (Ap (SF i) o) instance Semigroup o => Semigroup (SF i o)
deriving via (Ap (SF i) o) instance Monoid o    => Monoid    (SF i o)

deriving via (Ap Event o) instance Semigroup o => Semigroup (Event o)
deriving via (Ap Event o) instance Monoid o    => Monoid    (Event o)

-- | Perform the given action for a single frame, rendering the next step of
-- the Swont for that frame.
momentary :: Semigroup o => o -> Swont i o ()
momentary what = Swont $ cont $ \ f ->
  dSwitch
    (proc i -> do
      io <- constant what -< ()
      k  <- f () -< i
      ev <- now () -< ()
      returnA -< (io <> k, ev)
    )
    $ const $ f ()

data Resumption s o = Resumption
  { r_state  :: !s
  , r_output :: !o
  , r_stop  :: !(Event ())
  }
  deriving stock Functor

instance Bifunctor Resumption where
  bimap fab fcd (Resumption a c ev) = Resumption
    { r_state = fab a
    , r_output = fcd c
    , r_stop = ev
    }

-- | A 'Resumable' is a signal function with state. The final state is returned
-- by 'runResumable', meaning you can resume it exactly where you left off.
newtype Resumable s i o = Resumable
  { unResumable :: SF (s, i) (Resumption s o)
  }
  deriving stock Functor

runResumable :: s -> Resumable s i o -> Swont i o s
runResumable s0 (Resumable sf) = swont $ loopPre s0 $
  proc is -> do
    Resumption s' o ev <- sf -< swap is
    returnA -< ((o, s' <$ ev), s')

