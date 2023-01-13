module Engine.Prelude
  ( module Engine.Prelude
  , module X
  ) where

import Core.Types as X
import Data.Maybe as X (mapMaybe, fromMaybe, listToMaybe)
import Data.Foldable as X (for_, traverse_, asum, find)
import Data.Generics.Labels ()
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.Word as X
import Engine.Drawing as X
import Engine.FRP as X hiding (norm)
import Engine.Types as X
import SDL as X (quadrance, qd, norm, _xy, _xyz)
import SDL.Mixer as X (Chunk, Music)
import Engine.Utils as X


traceF :: Show b => (a -> b) -> a -> a
traceF f a = trace (show $ f a) a


traceFX :: Show b => String -> (a -> b) -> a -> a
traceFX herald f a = trace (mappend (herald <> ": ") . show $ f a) a

