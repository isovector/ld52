module Resources where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Traversable
import qualified SDL.Image as Image
import           System.FilePath
import           Types
import qualified SDL.Mixer as Mixer


loadResources :: Engine -> IO Resources
loadResources engine = do
  let renderer = e_renderer engine

  textures <- fmap M.fromList $
    for [minBound @GameTexture .. maxBound] $ \tx-> do
      let fp = texturePath tx
      fmap (tx, ) $ Image.loadTexture renderer fp

  sounds <- fmap M.fromList $
    for [minBound @Sound .. maxBound] $ \tx-> do
      let fp = soundPath tx
      fmap (tx, ) $ Mixer.load fp

  pure $ Resources
    { r_engine = engine
    , r_textures = fromMaybe (error "missing texture") . flip M.lookup textures
    , r_sounds = fromMaybe (error "missing sound") . flip M.lookup sounds
    }


textureName :: GameTexture -> FilePath
textureName NintendoLogo = "nintendo"

texturePath :: GameTexture -> FilePath
texturePath t = "./resources/textures" </> textureName t <.> "png"


soundName :: Sound -> String
soundName NintendoSound = "ding"

soundPath :: Sound -> FilePath
soundPath t = "./resources/sounds" </> soundName t <.> "wav"

