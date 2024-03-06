-- problem 1.1
findNum :: Integer -> [Integer] -> Bool
findNum _ [] = False
findNum n(x:xs)
  | n == x  = True
  | otherwise = findNum n xs

findNum' :: Integer -> [Integer] -> Bool
findNum' n xs = foldr (\x acc -> acc || (x == n)) False xs

-- problem 1.2
exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists p (x:xs)
  | p x       = True
  | otherwise = exists p xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' p xs = foldr (\x acc -> acc || p x) False xs

-- problem 1.3
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = x : noDups (filter (/= x) xs)

noDups' :: Eq a => [a] -> [a]
noDups' = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- problem 1.4
countLong :: Integer -> [String] -> Integer
countLong _ [] = 0
countLong x (s:ss)
  | fromIntegral (length s) > x = 1 + countLong x ss
  | otherwise = countLong x ss

countLong' :: Integer -> [String] -> Integer
countLong' x = foldr (\s acc -> if fromIntegral (length s) > x then 1 + acc else acc) 0

-- problem 1.5
bindList :: (a -> [b]) -> [a] -> [b]
bindList _ [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f = foldr (\x acc -> f x ++ acc) []

-- PROBLEM 2

-- PROBLEM 2.1
data LTree a = LLeaf a | LNode a (LTree a) (LTree a) 
  deriving Show

-- problem 2.1.1
getLeaves :: LTree a -> [a]
getLeaves (LLeaf x) = [x]
getLeaves (LNode _ l r) = getLeaves l ++ getLeaves r

-- problem 2.1.2
countNodes :: LTree a -> Integer
countNodes (LLeaf _) = 0
countNodes (LNode _ l r) = 1 + countNodes l + countNodes r

-- problem 2.1.3
sumTree :: LTree Integer -> Integer
sumTree (LLeaf x) = x
sumTree (LNode x l r) = x + sumTree l + sumTree r

-- problem 2.1.4
occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves p (LLeaf x) = p x
occursInLeaves p (LNode _ l r) = occursInLeaves p l || occursInLeaves p r

-- problem 2.1.5
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover x (LLeaf y) = x == y
checkNoCover x (LNode y l r) 
    | x == y = False
    | otherwise = checkNoCover x l || checkNoCover x r

-- PROBLEM 2.2
foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) = comb y (foldTree comb base t1)
                                            (foldTree comb base t2)

-- problem 2.2.1
getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\_ l r -> l ++ r) (\x -> [x])

-- problem 2.2.2
countNodes' :: LTree a -> Integer
countNodes' = foldTree (\_ l r -> 1 + l + r) (\_ -> 0)

-- problem 2.2.3
sumTree' :: LTree Integer -> Integer
sumTree' = foldTree (\x l r -> x + l + r) id

-- problem 2.2.4
occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' p = foldTree (\_ l r -> l || r) p

-- problem 2.2.5
checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' x = foldTree (\y l r -> if x == y then False else l || r) (==x)
