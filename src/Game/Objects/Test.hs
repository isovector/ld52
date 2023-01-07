module Game.Objects.Test where

import Drawing
import FRP
import Types

shrapnel :: Int -> V2 WorldPos -> Double -> Object
shrapnel _n pos0 theta = Object noObjectMeta $ arr oi_frameInfo >>> loopPre pos0
  ( proc (fi, pos) -> do
    die <- never -< () -- after 2 () -< ()
    let dt = fi_dt fi
    let pos' = pos + coerce (V2 (cos theta) (sin theta) ^* 50 ^* dt)
    returnA -<
      ( ObjectOutput
          { oo_events = ObjectEvents die noEvent noEvent noEvent
          , oo_render
              = drawFilledRect (V4 255 0 0 255)
              $ flip Rectangle 3
              $ P pos'
          , oo_pos = pos'
          }
      , pos'
      )
  )


grenade :: Object
grenade = Object noObjectMeta $
  timedSequence
    (proc _ -> do
      die <- after 3 () -< ()
      sp <- now () -< ()
      returnA -<
         ObjectOutput (ObjectEvents
            die
            (tag sp $ do
              n <- [id @Int 0 .. 5]
              pure $ shrapnel n pos $ 2 * pi / 6 * fromIntegral n
            )
            noEvent
            ([NintendoSound] <$ die)
            )
            (drawFilledRect (V4 255 0 0 255) $ flip Rectangle 8 $ P pos)
            pos

    ) 0.5
    $ do
      col <- [V4 255 0 0 255, V4 0 255 0 255, V4 0 0 255 255]
      pure
        $ constant
        $ ObjectOutput
            (ObjectEvents noEvent noEvent noEvent noEvent)
            (drawFilledRect col $ flip Rectangle 8 $ P pos)
            pos
  where
    pos = V2 50 50

