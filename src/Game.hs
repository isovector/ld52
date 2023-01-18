{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           Engine.Globals
import           Engine.ObjectRouter
import           Engine.Prelude
import           Game.Objects.CollectPowerup (powerupRenderable)
import           Game.World (drawLevel)
import Numeric (showFFloat)


#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr (uncurry addStaticObject) (ObjectMap (DynamicId 0) mempty gs mempty)
  $ M.toList
  $ l_defaultObjs $ gs_currentLevel gs


game :: GlobalState -> SF RawFrameInfo ((Camera, Renderable), Event ())
game gs0 =
  proc rfi -> do
    (cam, objs, to_draw) <-
      renderObjects gs0 (tileToPos $ V2 19 50) (initialObjs gs0)
          -< rfi
    let gs = objm_globalState objs
        levelsz = fmap (fromIntegral . getPixel)
                $ r_size
                $ l_bounds
                $ gs_currentLevel gs

    let drawPowerup :: PowerupType -> V2 ScreenPos -> Renderable
        drawPowerup pt (coerce -> pos) = atScreenPos $ mconcat $
          [ drawOriginRect (V4 255 255 255 16) (mkCenterdOriginRect 20) pos
          , bool
              mempty
              ( atScreenPos $
                  drawSpriteStretched
                    (setCenterOrigin $ global_textures $ powerupRenderable pt)
                    pos
                    0
                    (pure False)
                    0.2
              ) $ S.member pt $ gs_inventory $ gameState gs
          ]

    bg <- arr $ uncurry drawLevel -< (gs_layerset gs, gs_currentLevel gs)
    t <- localTime -< ()

    let eggs = gs_coins $ gs_gameState gs
        won = gs_end $ gs_gameState gs
        got_eggs = eggs == 5
    game_won <- edge -< won
    all_eggs <- edge -< got_eggs
    perfect <- edge -< won && got_eggs && gs_deaths (gs_gameState gs) == 0
    egg_time <- hold Nothing -< Just t <$ all_eggs
    end_time <- hold Nothing -< Just t <$ game_won
    perfect_time <- hold Nothing -< Just t <$ perfect

    reset <- edge -< c_full_restart $ controls rfi

    returnA -< (, reset) $
      ( cam
      , mconcat
          [ drawBackgroundColor (V4 46 90 137 255)
          , drawParallax levelsz Parallax0 3
          , drawParallax levelsz Parallax1 4
          , drawParallax levelsz Parallax2 5
          , bg
          , to_draw
          , drawPowerup PowerupWarpBall $ ui_box_pos (-17)
          , drawPowerup PowerupTotsugeki $ ui_box_pos (17)
          , ifA (eggs > 0) $ atScreenPos
              $ drawText 8
                  (V3 255 255 0)
                  (show $ gs_coins $ gameState gs)
              $ V2 10 10
          , flip foldMap end_time $ \et -> atScreenPos
              $ drawText 8
                  (V3 255 255 255)
                  (formatTime et)
              $ V2 240 10
          , flip foldMap egg_time $ \et -> atScreenPos
              $ drawText 8
                  (V3 255 255 0)
                  (formatTime et)
              $ V2 240 20
          , flip foldMap perfect_time $ \et -> atScreenPos
              $ drawText 8
                  (V3 255 0 0)
                  (formatTime et)
              $ V2 240 30
          ]
        )
  where
    ui_box_pos dx =
      (logicalSize / 2)
        & _x +~ dx
        & _y .~ 20

formatTime :: Time -> String
formatTime t =
  let (tsecs, tmils) = properFraction @_ @Int t
      mins = lpad 2 '0' $ show $ div tsecs 60
      secs = lpad 2 '0' $ show $ mod tsecs 60
   in mins <> (':' : secs <> ('.' : drop 2 (showFFloat (Just 3) tmils "")))

lpad :: Int -> Char -> String -> String
lpad n c s
  | let l = length s
  , l < n = replicate (n - l) c <> s
  | otherwise = s

initialGlobalState :: WorldName -> GlobalState
initialGlobalState w
  = GlobalState
      (w_levels (global_worlds w) M.! "AutoLayer")
      (S.fromList [Layer3])
      (GameState 0 mempty False 0)

#endif

