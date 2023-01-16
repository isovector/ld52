module Game.Common
  ( module Engine.Prelude
  , module Game.Common
  , module Engine.Common
  ) where

import           Data.Maybe (isNothing)
import           Data.Monoid
import qualified Data.Set as S
import           Engine.Common
import           Engine.Prelude


onHitBy :: ObjectTag -> ObjectInput -> Event ObjectId
onHitBy otag oi = do
  hits <- oie_hit $ oi_events oi
  case find (any $ S.member otag . os_tags) hits of
    Just x0 -> pure $ fst x0
    Nothing -> noEvent

playerHitRectObj'
    :: (SF ObjectInput ObjectEvents)
    -> OriginRect Double
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj' msg ore col pos =
  playerHitRectObj (msg >>> arr (, ())) ore (const $ drawOriginRect col ore) pos

playerHitRectObj
    :: (SF ObjectInput (ObjectEvents, a))
    -> OriginRect Double
    -> (a -> V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore r pos =
  proc oi -> do
    (evs, a) <- msg -< oi

    returnA -< ObjectOutput
      { oo_events = evs
      , oo_render = r a $ os_pos $ oi_state oi
      , oo_state = (noObjectState pos)
          { os_collision = Just $ coerce ore
          }
      }

getCollisionMap :: GlobalState -> CollisionPurpose -> V2 WorldPos -> Bool
getCollisionMap gs = do
  let lev = gs_currentLevel gs
      layers = gs_layerset gs

  \purpose -> getAny
            . foldMap ((fmap Any .) . l_hitmap lev) layers purpose
            . posToTile


charging :: Time -> SF ObjectInput Bool -> SF ObjectInput (Double, Event Double)
charging dur while = proc oi -> do
  maxed <- after dur 1 -< ()
  t <- sscan (+) 0 -< fi_dt $ oi_frameInfo oi
  x <- while -< oi
  released <- edge -< x
  let prog = t / dur

  let done = mergeEvents
              [ maxed
              , prog <$ released
              ]

  returnA -< (prog , done)


getCoinResponse :: SF (Event a) ObjectEvents
getCoinResponse = response #oe_game_message $ const [AddCoin]


addInventoryResponse :: PowerupType -> SF (Event a) ObjectEvents
addInventoryResponse
  = response #oe_game_message
  . const
  . pure
  . AddInventory

data RateLimited a = RateLimited
  { rl_cooldown_left :: Maybe Time
  , rl_on_refresh :: Event ()
  , rl_data :: a
  }
  deriving (Eq, Ord, Show, Generic)

rl_available :: RateLimited a -> Bool
rl_available = isNothing . rl_cooldown_left


rateLimit :: Time -> SF (Event a, b) c -> SF (Event a, b) (RateLimited c)
rateLimit cooldown sf = loopPre 0 $ proc ((ev, b), last_ok) -> do
  t <- time -< ()
  let next_alive = t + cooldown
      ok = t >= last_ok

  let actually_die = whenE ok ev
  respawn_at <- hold 0 -< next_alive <$ actually_die

  let alive = respawn_at <= t

  out <- sf -< (actually_die, b)

  respawn <- edge -< respawn_at <= t
  returnA -< (RateLimited (bool (Just $ respawn_at - t) Nothing alive)  respawn out, respawn_at)

dieOnPlayerDeath :: Object -> Object
dieOnPlayerDeath obj = proc oi -> do
  oo <- obj -< oi
  let player_death = listenInbox (preview #_PlayerDeath . snd) $ oi_events oi
  returnA -< oo & #oo_events . #oe_die <>~ player_death

