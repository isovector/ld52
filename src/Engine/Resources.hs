{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}

module Engine.Resources where

import qualified Data.Map as M
import           Data.Traversable (for)
import           Engine.Types (Engine)
import           System.Environment.Blank (getEnv)
import           System.FilePath


class (Ord key, Enum key, Bounded key)
      => IsResource key res
       | key -> res
       where
  resourceFolder :: String
  resourceExt :: String
  resourceName :: key -> String
  load :: key -> Engine -> FilePath -> IO res

resourceRootPath :: IO FilePath
resourceRootPath =
  maybe "resources" (</> "usr/share/ld52-exe/resources") <$> getEnv "APPDIR"

loadResource
    :: forall key res
     . IsResource key res
    => FilePath -> Engine -> IO (key -> res)
loadResource rpath engine = do
  m <- fmap M.fromList $
    for [minBound @key .. maxBound] $ \k ->
      fmap (k, ) $ load @_ @res k engine $
        rpath </> resourceFolder @key @res </>
          resourceName k <.> resourceExt @key @res
  pure $ \k -> m M.! k

