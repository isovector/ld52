module Game.Objects.Test where

import Drawing
import FRP
import Types
import Utils (noObjectState)

shrapnel :: Int -> V2 WorldPos -> Double -> Object
shrapnel _n pos0 theta = arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- never -< () -- after 2 () -< ()
    let dt = fi_dt fi
    let pos' = pos + coerce (V2 (cos theta) (sin theta) ^* 50 ^* dt)
    returnA -<
      ( ObjectOutput
          { oo_events =
              mempty
                { oe_die = die
                }
          , oo_render
              = drawFilledRect (V4 255 0 0 255)
              $ flip Rectangle 3
              $ P pos'
          , oo_state = noObjectState pos'
          }
      , pos'
      )
  )


grenade :: V2 WorldPos -> Double -> Object
grenade pos life =
  timedSequence
    (proc _ -> do
      die <- after life () -< ()
      sp <- now () -< ()
      returnA -<
         ObjectOutput (
          mempty
            { oe_die = die
            , oe_spawn = tag sp $ do
                n <- [id @Int 0 .. 5]
                pure $ shrapnel n pos $ 2 * pi / 6 * fromIntegral n
            }
            )
            (drawFilledRect (V4 255 0 0 255) $ flip Rectangle 8 $ P pos)
            (noObjectState pos)

    ) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput
            mempty
            (drawFilledRect col $ flip Rectangle 8 $ P pos)
            (noObjectState pos)

