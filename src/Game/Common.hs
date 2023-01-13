module Game.Common
  ( module Engine.Prelude
  , module Game.Common
  , module Engine.Common
  ) where

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

