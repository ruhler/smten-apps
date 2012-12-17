
module Map where

-- Implementation of Map which should work in seri.
data Map k v = Map {
    map_toList :: [(k, v)]
}

map_lookup :: (Eq k) => k -> Map k v -> Maybe v
map_lookup k (Map m) = lookup k m

map_insert :: k -> v -> Map k v -> Map k v
map_insert k v (Map m) = Map $ (k, v):m

map_empty :: Map k v
map_empty = Map []

map_fromList :: [(k, v)] -> Map k v
map_fromList = Map

