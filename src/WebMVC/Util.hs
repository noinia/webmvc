module WebMVC.Util
  ( (=:)
  ) where

import qualified Data.Map as Map

-- | Shorthand for creating a singleton Map
(=:)   :: k -> a -> Map.Map k a
k =: v = Map.singleton k v
