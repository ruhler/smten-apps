
-- Implementation of binary balanced tree based on Data.Map source from
-- haskell library.
module Map where


-- Implementation of Map which should work in seri.
data Map k a = Tip
             | Bin Size k a (Map k a) (Map k a)

type Size = Integer

instance (Show k, Show a) => Show (Map k a) where
    show m = show (map_toList m)

map_lookup :: (Eq k, Ord k) => k -> Map k v -> Maybe v
map_lookup k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r ->
        case compare k kx of
          LT -> map_lookup k l
          GT -> map_lookup k r
          EQ -> Just x
      
map_insert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v
map_insert kx x t =
  case t of
    Tip -> map_singleton kx x
    Bin sz ky y l r ->
        case compare kx ky of
           LT -> balance ky y (map_insert kx x l) r
           GT -> balance ky y l (map_insert kx x r)
           EQ -> Bin sz kx x l r

delta :: Integer
delta = 5

ratio :: Integer
ratio = 2

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r =
  let sizeL = map_size l
      sizeR = map_size r
      sizeX = sizeL + sizeR + 1
  in if (sizeL + sizeR <= 1)
        then Bin sizeX k x l r
        else if (sizeR >= delta*sizeL)
                then rotateL k x l r
                else if (sizeL >= delta*sizeR)
                    then rotateR k x l r
                    else Bin sizeX k x l r

rotateL :: a -> b -> Map a b -> Map a b -> Map a b
rotateL k x l r@(Bin _ _ _ ly ry) =
  if (map_size ly < ratio * map_size ry)
     then singleL k x l r
     else doubleL k x l r
rotateL _ _ _ _ = error "rotateL Tip"

rotateR :: a -> b -> Map a b -> Map a b -> Map a b
rotateR k x l@(Bin _ _ _ ly ry) r =
  if (map_size ry < ratio * map_size ly)
      then singleR k x l r
      else doubleR k x l r
rotateR _ _ _ _ = error "rotateR Tip"

singleL :: a -> b -> Map a b -> Map a b -> Map a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3) = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ _ = error "singleL Tip"

singleR :: a -> b -> Map a b -> Map a b -> Map a b
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3 = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ _ _ = error "singleR Tip"

doubleL :: a -> b -> Map a b -> Map a b -> Map a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR :: a -> b -> Map a b -> Map a b -> Map a b
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (map_size l + map_size r + 1) k x l r


map_size :: Map k v -> Size
map_size Tip = 0
map_size (Bin sz _ _ _ _) = sz

map_empty :: Map k v
map_empty = Tip

map_singleton :: k -> v -> Map k v
map_singleton k v = Bin 1 k v map_empty map_empty

map_fromList :: (Eq k, Ord k) => [(k, v)] -> Map k v
map_fromList ((k, v):xs) = map_insert k v (map_fromList xs)
map_fromList _ = map_empty

map_toList :: Map k v -> [(k, v)]
map_toList (Bin _ k v a b) = concat [map_toList a, [(k, v)], map_toList b]
map_toList _ = []

