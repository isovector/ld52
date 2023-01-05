module Resources where

import Types


loadResources :: Engine -> IO Resources
loadResources = pure . Resources

