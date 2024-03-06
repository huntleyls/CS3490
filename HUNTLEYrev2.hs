-- Problem1
minList :: [Integer] -> Integer
minList [] = 0
minList(x:xs) = foldl min x xs

-- Problem 2
addAbs :: [Integer] -> Integer
addAbs [] = 0
addAbs (x:xs) = abs x + addAbs xs

-- Problem 3 
existsOdd :: [Integer] -> Bool
existsOdd xs = any odd xs

-- Problem 4
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing                  
findOdd (x:xs)                        
    | odd x      = Just x             
    | otherwise  = findOdd xs   

-- Problem 5 
removeEmpty :: [String] -> [String]
removeEmpty strs = [str | str <- strs, not (null str)]

-- Problem 6
subtractEach :: [(Integer, Integer)] -> [Integer]
subtractEach = map (\(a, b) -> a - b)

-- Problem 7
makeGreeting :: Maybe String -> String
makeGreeting (Just name) = "Hello, " ++ name ++ "!"
makeGreeting Nothing     = "Hello!"

-- Problem 8
catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter isJust
  where
    isJust (Just _) = True
    isJust _        = False

-- Problem 9
classify :: [Either a b] -> ([a], [b])
classify xs = (lefts xs, rights xs)
  where
    lefts  = map (\(Left x) -> x) . filter isLeft
    rights = map (\(Right x) -> x) . filter isRight
    
    isLeft (Left _) = True
    isLeft _        = False
    
    isRight (Right _) = True
    isRight _         = False

-- Problem 10
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
