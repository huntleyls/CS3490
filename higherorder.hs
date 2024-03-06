--problem 1
mapPair :: (a -> b -> c) -> [(a, b)] -> [c]
mapPair f pairs = map (\(x, y) -> f x y) pairs
--problem 2
mapPair' :: (a -> b -> c) -> [(b, a)] -> [c]
mapPair' f pairs = map (\(x, y) -> f y x) pairs
