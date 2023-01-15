module SDL.JuicyPixels where

import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
import           SDL (Texture, Surface)
import           SDL.Vect
import qualified SDL.Video.Renderer as R

loadJuicySurface :: FilePath -> IO Surface
loadJuicySurface fp = do
  Right img <- P.readImage fp
  let rgba8 = P.convertRGBA8 img
  let width = P.imageWidth rgba8
      height = P.imageHeight rgba8
      iData = P.imageData rgba8
  rawData <- V.thaw iData
  R.createRGBSurfaceFrom
      rawData
      (V2 (CInt $ fromIntegral width) (CInt $ fromIntegral height))
      (4 * (CInt $ fromIntegral width))
      R.ABGR8888


loadJuicyTexture :: R.Renderer -> FilePath -> IO Texture
loadJuicyTexture r fp = loadJuicySurface fp >>= R.createTextureFromSurface r

