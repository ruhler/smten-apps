
module Map where

-- Implementation of Map which should work in seri.
data Map k v =
    EmptyMap
    -- Elements on the left are < this key. Elements on the right >.
  | Map k v (Map k v) (Map k v)

map_lookup :: (Eq k, Ord k) => k -> Map k v -> Maybe v
map_lookup x (Map k v a b) =
    if (x == k)
       then Just v
       else if (x < k)
                then map_lookup x a
                else map_lookup x b
map_lookup _ _ = Nothing

map_insert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v
map_insert nk nv (Map k v a b) = 
    if (nk == k)
        then Map nk nv a b
        else if (nk < k)
                then Map k v (map_insert nk nv a) b
                else Map k v a (map_insert nk nv b)
map_insert k v EmptyMap = Map k v map_empty map_empty

map_empty :: Map k v
map_empty = EmptyMap

map_fromList :: (Eq k, Ord k) => [(k, v)] -> Map k v
map_fromList ((k, v):xs) = map_insert k v (map_fromList xs)
map_fromList _ = map_empty

map_toList :: Map k v -> [(k, v)]
map_toList (Map k v a b) = concat [map_toList a, [(k, v)], map_toList b]
map_toList _ = []

