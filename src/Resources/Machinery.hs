{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}

module Resources.Machinery where

import qualified Data.Map as M
import           Data.Traversable (for)
import           System.FilePath
import           Types (Engine)


class (Ord key, Enum key, Bounded key)
      => IsResource key res
       | key -> res
       where
  resourceFolder :: String
  resourceExt :: String
  resourceName :: key -> String
  load :: Engine -> FilePath -> IO res


loadResource
    :: forall key res
     . IsResource key res
    => Engine -> IO (key -> res)
loadResource engine = do
  m <- fmap M.fromList $
    for [minBound @key .. maxBound] $ \k ->
      fmap (k, ) $ load @key @res engine $
        "resources" </> resourceFolder @key @res </>
          resourceName k <.> resourceExt @key @res
  pure $ \k -> m M.! k

